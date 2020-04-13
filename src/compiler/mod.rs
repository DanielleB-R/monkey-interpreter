mod symbol;
#[cfg(test)]
mod tests;

use crate::ast::{Expression, Node, Operator, Statement};
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
        Self {
            constants: Default::default(),
            symbol_table: Default::default(),
            scopes: vec![Default::default()],
            scope_index: 0,
        }
    }
}

#[derive(Default, Clone)]
pub struct CompilerState {
    constants: Vec<Object>,
    symbol_table: Box<symbol::SymbolTable>,
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

    pub fn compile(&mut self, node: Node) -> Result<(), CompileError> {
        match node {
            Node::Program(p) => {
                for stmt in p.statements {
                    self.compile(stmt.into())?;
                }
            }
            Node::Statement(stmt) => match stmt {
                Statement::Expr(e) => {
                    self.compile(e.expression.into())?;
                    self.emit(Opcode::Pop, &[]);
                }
                Statement::Block(block) => {
                    for stmt in block.statements {
                        self.compile(stmt.into())?;
                    }
                }
                Statement::Let(let_stmt) => {
                    self.compile(let_stmt.value.into())?;

                    let symbol = self.symbol_table.define(&let_stmt.name.value);
                    self.emit(
                        match symbol.scope {
                            Scope::Global => Opcode::SetGlobal,
                            Scope::Local => Opcode::SetLocal,
                        },
                        &[symbol.index],
                    );
                }
                Statement::Return(ret_stmt) => {
                    self.compile(ret_stmt.return_value.into())?;
                    self.emit(Opcode::ReturnValue, &[]);
                }
            },
            Node::Expression(expr) => match expr {
                Expression::Identifier(ident) => match self.symbol_table.resolve(&ident.value) {
                    Some(symbol) => {
                        self.emit(
                            match symbol.scope {
                                Scope::Global => Opcode::GetGlobal,
                                Scope::Local => Opcode::GetLocal,
                            },
                            &[symbol.index],
                        );
                    }
                    None => {
                        return Err(CompileError::UndefinedIdentifier {
                            name: ident.value.clone(),
                        });
                    }
                },
                Expression::Infix(infix) => {
                    if infix.operator == Operator::LT {
                        self.compile((*infix.right).into())?;
                        self.compile((*infix.left).into())?;
                        self.emit(Opcode::GreaterThan, &[]);
                        return Ok(());
                    }

                    self.compile((*infix.left).into())?;
                    self.compile((*infix.right).into())?;

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
                    self.compile((*prefix.right).into())?;

                    match prefix.operator {
                        Operator::Bang => self.emit(Opcode::Bang, &[]),
                        Operator::Minus => self.emit(Opcode::Minus, &[]),
                        op => return Err(CompileError::UnknownOperator { op }),
                    };
                }
                Expression::Index(ind) => {
                    self.compile((*ind.left).into())?;
                    self.compile((*ind.index).into())?;
                    self.emit(Opcode::Index, &[]);
                }
                Expression::IntegerLiteral(int) => {
                    let constant = self.add_constant(int.value.into());
                    self.emit(Opcode::Constant, &[constant]);
                }
                Expression::String(s) => {
                    let constant = self.add_constant(s.value.into());
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
                    for expr in array.elements {
                        self.compile(expr.into())?;
                    }
                    self.emit(Opcode::Array, &[len]);
                }
                Expression::Hash(hash) => {
                    let len = hash.pairs.len() * 2;
                    for (key, value) in hash.pairs {
                        self.compile(key.into())?;
                        self.compile(value.into())?;
                    }

                    self.emit(Opcode::Hash, &[len as isize]);
                }
                Expression::Function(f) => {
                    self.enter_scope();

                    let num_params = f.parameters.len();
                    for param in f.parameters {
                        self.symbol_table.define(&param.value);
                    }

                    self.compile(Statement::Block(f.body).into())?;

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
                    self.emit(Opcode::Constant, &[const_index]);
                }
                Expression::Call(c) => {
                    self.compile((*c.function).into())?;

                    let arg_count = c.arguments.len();
                    for arg in c.arguments {
                        self.compile(arg.into())?;
                    }

                    self.emit(Opcode::Call, &[arg_count as isize]);
                }
                Expression::If(expr) => {
                    self.compile((*expr.condition).into())?;

                    let jump_falsy_pos = self
                        .emit(Opcode::JumpFalsy, &[9999])
                        .expect("Jump failed to emit");

                    self.compile(Statement::Block(expr.consequence).into())?;

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
                            self.compile(Statement::Block(alt).into())?;

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
        &mut self.scopes[self.scope_index].instructions
    }

    pub fn emit(&mut self, op: Opcode, operands: &[isize]) -> Option<usize> {
        let ins = code::make(op, operands)?;
        let pos = self.add_instruction(ins);

        self.set_last_instruction(op, pos);
        Some(pos)
    }

    pub fn add_instruction(&mut self, ins: Instructions) -> usize {
        let instructions = self.current_instructions();
        let pos = instructions.len();
        instructions.append(ins);
        pos
    }

    fn set_last_instruction(&mut self, opcode: Opcode, position: usize) {
        let mut scope = &mut self.scopes[self.scope_index];
        scope.previous = scope.last;
        scope.last = EmittedInstruction { opcode, position };
    }

    fn last_instruction_is(&self, op: Opcode) -> bool {
        let scope = &self.scopes[self.scope_index];
        scope.instructions.len() != 0 && scope.last.opcode == op
    }

    fn remove_last_pop(&mut self) {
        let mut scope = &mut self.scopes[self.scope_index];
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
        let mut scope = &mut self.scopes[self.scope_index];
        let last_pos = scope.last.position;
        scope
            .instructions
            .replace_instruction(last_pos, code::make(Opcode::ReturnValue, &[]).unwrap());
        scope.last.opcode = Opcode::ReturnValue;
    }

    fn enter_scope(&mut self) {
        self.scopes.push(Default::default());
        self.scope_index += 1;
        self.symbol_table = symbol::SymbolTable::enclosing(self.symbol_table.clone());
    }

    fn leave_scope(&mut self) -> Instructions {
        let scope = self.scopes.pop().unwrap();
        self.scope_index -= 1;
        self.symbol_table = self.symbol_table.clone().unenclose();
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
