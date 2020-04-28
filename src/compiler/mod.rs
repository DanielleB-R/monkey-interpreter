mod symbol;
#[cfg(test)]
mod tests;

use crate::ast::{Expression, Node, Operator, Statement};
use crate::builtins::BUILTINS;
use crate::code::{self, BytecodeError, Instructions, Opcode};
use crate::object::{CompiledFunction, Object};
use custom_error::custom_error;
use std::convert::TryInto;
use symbol::Scope;

custom_error! {
    pub CompileError

    InvalidBytecode{source: BytecodeError} = "invalid bytecode: {source}",
    UnknownOperator{op: Operator} = "unknown operator {op}",
    UndefinedIdentifier{name: String} = "undefined variable {name}",
}

#[derive(Debug, Clone, Copy, Default)]
pub struct EmittedInstruction {
    pub opcode: Opcode,
    pub position: usize,
}

#[derive(Debug, Clone, Default)]
pub struct CompilationScope {
    instructions: Instructions,
    last: EmittedInstruction,
    previous: EmittedInstruction,
}

#[derive(Debug, Clone)]
pub struct Compiler {
    constants: Vec<Object>,

    symbol_table: Box<symbol::SymbolTable>,

    scopes: Vec<CompilationScope>,
    scope_index: usize,
}

impl Default for Compiler {
    fn default() -> Self {
        let mut symbol_table = Box::new(symbol::SymbolTable::default());

        for (i, (name, _)) in BUILTINS.iter().enumerate() {
            symbol_table.define_builtin(i as isize, name);
        }

        Self {
            constants: Default::default(),
            symbol_table,
            scopes: vec![Default::default()],
            scope_index: 0,
        }
    }
}

#[derive(Default, Clone)]
pub struct CompilerState {
    constants: Vec<Object>,
    pub symbol_table: Box<symbol::SymbolTable>,
}

impl From<CompilerState> for Compiler {
    fn from(state: CompilerState) -> Self {
        Self {
            constants: state.constants,
            symbol_table: state.symbol_table,
            scopes: vec![Default::default()],
            scope_index: 0,
        }
    }
}

impl Compiler {
    pub fn save_state(&self) -> CompilerState {
        CompilerState {
            constants: self.constants.clone(),
            symbol_table: self.symbol_table.clone(),
        }
    }

    pub fn compile<T: Into<Node>>(&mut self, node: T) -> Result<(), CompileError> {
        match node.into() {
            Node::Program(p) => {
                p.statements.into_iter().try_for_each(|s| self.compile(s))?;
            }
            Node::Statement(stmt) => match stmt {
                Statement::Expr(expr) => {
                    self.compile(expr)?;
                    self.emit(Opcode::Pop, &[]);
                }
                Statement::Block(block) => {
                    block
                        .statements
                        .into_iter()
                        .try_for_each(|s| self.compile(s))?;
                }
                Statement::Let(let_stmt) => {
                    self.compile(let_stmt.value)?;

                    let symbol = self.symbol_table.define(&let_stmt.name.value);
                    self.emit(
                        match symbol.scope {
                            Scope::Global => Opcode::SetGlobal,
                            Scope::Local => Opcode::SetLocal,
                            Scope::Builtin => panic!("define should never return a builtin"),
                        },
                        &[symbol.index],
                    );
                }
                Statement::Return(return_value) => {
                    self.compile(return_value)?;
                    self.emit(Opcode::ReturnValue, &[]);
                }
            },
            Node::Expression(expr) => match expr {
                Expression::Identifier(ident) => match self.symbol_table.resolve(&ident.value) {
                    Some(symbol) => {
                        self.emit(Self::load_symbol_instruction(symbol), &[symbol.index]);
                    }
                    None => {
                        return Err(CompileError::UndefinedIdentifier {
                            name: ident.value.clone(),
                        });
                    }
                },
                Expression::Infix(infix) => {
                    if infix.operator == Operator::LT {
                        self.compile(*infix.right)?;
                        self.compile(*infix.left)?;
                        self.emit(Opcode::GreaterThan, &[]);
                        return Ok(());
                    }

                    self.compile(*infix.left)?;
                    self.compile(*infix.right)?;

                    match infix.operator {
                        Operator::Plus => self.emit(Opcode::Add, &[]),
                        Operator::Minus => self.emit(Opcode::Sub, &[]),
                        Operator::Asterisk => self.emit(Opcode::Mul, &[]),
                        Operator::Slash => self.emit(Opcode::Div, &[]),
                        Operator::GT => self.emit(Opcode::GreaterThan, &[]),
                        Operator::Eq => self.emit(Opcode::Equal, &[]),
                        Operator::NotEq => self.emit(Opcode::NotEqual, &[]),
                        op => return Err(CompileError::UnknownOperator { op }),
                    };
                }
                Expression::Prefix(prefix) => {
                    self.compile(*prefix.right)?;

                    match prefix.operator {
                        Operator::Bang => self.emit(Opcode::Bang, &[]),
                        Operator::Minus => self.emit(Opcode::Minus, &[]),
                        op => return Err(CompileError::UnknownOperator { op }),
                    };
                }
                Expression::Index(ind) => {
                    self.compile(*ind.left)?;
                    self.compile(*ind.index)?;
                    self.emit(Opcode::Index, &[]);
                }
                Expression::IntegerLiteral(int) => {
                    let constant = self.add_constant(int.into());
                    self.emit(Opcode::Constant, &[constant]);
                }
                Expression::String(s) => {
                    let constant = self.add_constant(s.into());
                    self.emit(Opcode::Constant, &[constant]);
                }
                Expression::Boolean(b) => {
                    if b.value {
                        self.emit(Opcode::True, &[]);
                    } else {
                        self.emit(Opcode::False, &[]);
                    }
                }
                Expression::Array(array) => {
                    let len = array.elements.len() as isize;
                    array
                        .elements
                        .into_iter()
                        .try_for_each(|expr| self.compile(expr))?;
                    self.emit(Opcode::Array, &[len]);
                }
                Expression::Hash(hash) => {
                    let len = hash.pairs.len() * 2;
                    for (key, value) in hash.pairs {
                        self.compile(key)?;
                        self.compile(value)?;
                    }

                    self.emit(Opcode::Hash, &[len as isize]);
                }
                Expression::Function(f) => {
                    self.enter_scope();

                    let num_params = f.parameters.len();
                    for param in f.parameters {
                        self.symbol_table.define(&param.value);
                    }

                    self.compile(f.body)?;

                    if self.last_instruction_is(Opcode::Pop) {
                        self.replace_last_pop_with_return();
                    }

                    if !self.last_instruction_is(Opcode::ReturnValue) {
                        self.emit(Opcode::Return, &[]);
                    }
                    let num_locals = self.symbol_table.num_definitions;
                    let instructions = self.leave_scope();

                    let const_index = self.add_constant(
                        CompiledFunction::new(instructions, num_locals, num_params).into(),
                    );
                    self.emit(Opcode::Closure, &[const_index, 0]);
                }
                Expression::Call(c) => {
                    self.compile(*c.function)?;

                    let arg_count = c.arguments.len();
                    for arg in c.arguments {
                        self.compile(arg)?;
                    }

                    self.emit(Opcode::Call, &[arg_count as isize]);
                }
                Expression::If(expr) => {
                    self.compile(*expr.condition)?;

                    let jump_falsy_pos = self
                        .emit(Opcode::JumpFalsy, &[9999])
                        .expect("Jump failed to emit");

                    self.compile(expr.consequence)?;

                    if self.last_instruction_is(Opcode::Pop) {
                        self.remove_last_pop();
                    }

                    let jump_pos = self
                        .emit(Opcode::Jump, &[9999])
                        .expect("Jump failed to emit");
                    let after_consequence_pos = self.current_instructions().len();
                    self.change_operand(jump_falsy_pos, after_consequence_pos as isize);
                    match expr.alternative {
                        Some(alt) => {
                            self.compile(alt)?;

                            if self.last_instruction_is(Opcode::Pop) {
                                self.remove_last_pop();
                            }
                        }
                        None => {
                            self.emit(Opcode::Null, &[]).expect("Null failed to emit");
                        }
                    };

                    let after_alternative_pos = self.current_instructions().len();
                    self.change_operand(jump_pos, after_alternative_pos as isize);
                }
                _ => println!("unimplemented"),
            },
        }

        Ok(())
    }

    pub fn add_constant(&mut self, obj: Object) -> isize {
        self.constants.push(obj);
        (self.constants.len() - 1) as isize
    }

    fn current_instructions(&mut self) -> &mut Instructions {
        &mut self.current_scope().instructions
    }

    fn current_scope(&mut self) -> &mut CompilationScope {
        &mut self.scopes[self.scope_index]
    }

    pub fn emit(&mut self, op: Opcode, operands: &[isize]) -> Option<usize> {
        let pos = self.add_instruction(code::make(op, operands)?);

        self.set_last_instruction(op, pos);
        Some(pos)
    }

    pub fn add_instruction(&mut self, ins: Instructions) -> usize {
        let instructions = self.current_instructions();
        let pos = instructions.len();
        instructions.append(ins);
        pos
    }

    fn load_symbol_instruction(symbol: symbol::Symbol) -> Opcode {
        match symbol.scope {
            Scope::Global => Opcode::GetGlobal,
            Scope::Local => Opcode::GetLocal,
            Scope::Builtin => Opcode::GetBuiltin,
        }
    }

    fn set_last_instruction(&mut self, opcode: Opcode, position: usize) {
        let mut scope = self.current_scope();
        scope.previous = scope.last;
        scope.last = EmittedInstruction { opcode, position };
    }

    fn last_instruction_is(&mut self, op: Opcode) -> bool {
        let scope = self.current_scope();
        scope.instructions.len() != 0 && scope.last.opcode == op
    }

    fn remove_last_pop(&mut self) {
        let mut scope = self.current_scope();
        scope.instructions.truncate(scope.last.position);
        scope.last = scope.previous;
    }

    fn change_operand(&mut self, pos: usize, operand: isize) {
        let instructions = self.current_instructions();
        instructions.replace_instruction(
            pos,
            code::make(
                instructions[pos]
                    .try_into()
                    .expect("Replacing invalid opcode"),
                &[operand],
            )
            .expect("Can only replace valid instructions"),
        );
    }

    fn replace_last_pop_with_return(&mut self) {
        let mut scope = self.current_scope();
        let last_pos = scope.last.position;
        scope
            .instructions
            .replace_instruction(last_pos, code::make(Opcode::ReturnValue, &[]).unwrap());
        scope.last.opcode = Opcode::ReturnValue;
    }

    fn enter_scope(&mut self) {
        self.scopes.push(Default::default());
        self.scope_index += 1;
        self.symbol_table = symbol::SymbolTable::enclosing(std::mem::take(&mut self.symbol_table));
    }

    fn leave_scope(&mut self) -> Instructions {
        let scope = self.scopes.pop().unwrap();
        self.scope_index -= 1;
        self.symbol_table = std::mem::take(&mut self.symbol_table).unenclose();
        scope.instructions
    }

    pub fn bytecode(self) -> Bytecode {
        Bytecode {
            instructions: self
                .scopes
                .into_iter()
                .nth(self.scope_index)
                .unwrap()
                .instructions,
            constants: self.constants,
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct Bytecode {
    pub instructions: Instructions,
    pub constants: Vec<Object>,
}
