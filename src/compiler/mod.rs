mod symbol;

use crate::ast::{Expression, Node, Operator, Statement};
use crate::code::{self, BytecodeError, Instructions, Opcode};
use crate::object::Object;
use custom_error::custom_error;
use std::convert::TryInto;

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
pub struct Compiler {
    pub instructions: Instructions,
    pub constants: Vec<Object>,

    last_instruction: EmittedInstruction,
    previous_instruction: EmittedInstruction,
    symbol_table: symbol::SymbolTable,
}

#[derive(Default, Clone)]
pub struct CompilerState {
    constants: Vec<Object>,
    symbol_table: symbol::SymbolTable,
}

impl From<CompilerState> for Compiler {
    fn from(state: CompilerState) -> Self {
        Self {
            instructions: Instructions::default(),
            constants: state.constants,

            last_instruction: EmittedInstruction::default(),
            previous_instruction: EmittedInstruction::default(),
            symbol_table: state.symbol_table,
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
                    self.emit(Opcode::SetGlobal, &[symbol.index]);
                }
                _ => println!("unimplemented"),
            },
            Node::Expression(expr) => match expr {
                Expression::Identifier(ident) => match self.symbol_table.resolve(&ident.value) {
                    Some(symbol) => {
                        self.emit(Opcode::GetGlobal, &[symbol.index]);
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
                Expression::If(expr) => {
                    self.compile((*expr.condition).into())?;

                    let jump_falsy_pos = self
                        .emit(Opcode::JumpFalsy, &[9999])
                        .expect("Jump failed to emit");

                    self.compile(Statement::Block(expr.consequence).into())?;

                    if self.last_instruction_is_pop() {
                        self.remove_last_pop();
                    }

                    let jump_pos = self
                        .emit(Opcode::Jump, &[9999])
                        .expect("Jump failed to emit");
                    let after_consequence_pos = self.instructions.len();
                    self.change_operand(jump_falsy_pos, after_consequence_pos as isize);
                    match expr.alternative {
                        Some(alt) => {
                            self.compile(Statement::Block(alt).into())?;

                            if self.last_instruction_is_pop() {
                                self.remove_last_pop();
                            }
                        }
                        None => {
                            self.emit(Opcode::Null, &[]).expect("Null failed to emit");
                        }
                    };

                    let after_alternative_pos = self.instructions.len();
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

    pub fn emit(&mut self, op: Opcode, operands: &[isize]) -> Option<usize> {
        let ins = code::make(op, operands)?;
        let pos = self.add_instruction(ins);

        self.set_last_instruction(op, pos);
        Some(pos)
    }

    fn set_last_instruction(&mut self, opcode: Opcode, position: usize) {
        self.previous_instruction = self.last_instruction;
        self.last_instruction = EmittedInstruction { opcode, position };
    }

    fn last_instruction_is_pop(&self) -> bool {
        self.last_instruction.opcode == Opcode::Pop
    }

    fn remove_last_pop(&mut self) {
        self.instructions.truncate(self.last_instruction.position);
        self.last_instruction = self.previous_instruction;
    }

    pub fn add_instruction(&mut self, ins: Instructions) -> usize {
        let pos = self.instructions.len();
        self.instructions.append(ins);
        pos
    }

    fn change_operand(&mut self, pos: usize, operand: isize) {
        self.instructions.replace_instruction(
            pos,
            code::make(
                self.instructions[pos]
                    .try_into()
                    .expect("Replacing invalid opcode"),
                &[operand],
            )
            .expect("Can only replace valid instructions"),
        );
    }

    pub fn bytecode(self) -> Bytecode {
        Bytecode {
            instructions: self.instructions,
            constants: self.constants,
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct Bytecode {
    pub instructions: Instructions,
    pub constants: Vec<Object>,
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast;
    use crate::code::{self, concat_instructions, Opcode};
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    #[test]
    fn test_integer_arithmetic() {
        let cases = vec![
            (
                "1 + 2",
                vec![1.into(), 2.into()],
                vec![
                    code::make(Opcode::Constant, &[0]).unwrap(),
                    code::make(Opcode::Constant, &[1]).unwrap(),
                    code::make(Opcode::Add, &[]).unwrap(),
                    code::make(Opcode::Pop, &[]).unwrap(),
                ],
            ),
            (
                "1 - 2",
                vec![1.into(), 2.into()],
                vec![
                    code::make(Opcode::Constant, &[0]).unwrap(),
                    code::make(Opcode::Constant, &[1]).unwrap(),
                    code::make(Opcode::Sub, &[]).unwrap(),
                    code::make(Opcode::Pop, &[]).unwrap(),
                ],
            ),
            (
                "1 * 2",
                vec![1.into(), 2.into()],
                vec![
                    code::make(Opcode::Constant, &[0]).unwrap(),
                    code::make(Opcode::Constant, &[1]).unwrap(),
                    code::make(Opcode::Mul, &[]).unwrap(),
                    code::make(Opcode::Pop, &[]).unwrap(),
                ],
            ),
            (
                "2 / 1",
                vec![2.into(), 1.into()],
                vec![
                    code::make(Opcode::Constant, &[0]).unwrap(),
                    code::make(Opcode::Constant, &[1]).unwrap(),
                    code::make(Opcode::Div, &[]).unwrap(),
                    code::make(Opcode::Pop, &[]).unwrap(),
                ],
            ),
            (
                "1; 2",
                vec![1.into(), 2.into()],
                vec![
                    code::make(Opcode::Constant, &[0]).unwrap(),
                    code::make(Opcode::Pop, &[]).unwrap(),
                    code::make(Opcode::Constant, &[1]).unwrap(),
                    code::make(Opcode::Pop, &[]).unwrap(),
                ],
            ),
            (
                "-1",
                vec![1.into()],
                vec![
                    code::make(Opcode::Constant, &[0]).unwrap(),
                    code::make(Opcode::Minus, &[]).unwrap(),
                    code::make(Opcode::Pop, &[]).unwrap(),
                ],
            ),
        ];

        run_compiler_tests(cases);
    }

    #[test]
    fn test_boolean_expressions() {
        run_compiler_tests(vec![
            (
                "true",
                vec![],
                vec![
                    code::make(Opcode::True, &[]).unwrap(),
                    code::make(Opcode::Pop, &[]).unwrap(),
                ],
            ),
            (
                "false",
                vec![],
                vec![
                    code::make(Opcode::False, &[]).unwrap(),
                    code::make(Opcode::Pop, &[]).unwrap(),
                ],
            ),
            (
                "1 > 2",
                vec![1.into(), 2.into()],
                vec![
                    code::make(Opcode::Constant, &[0]).unwrap(),
                    code::make(Opcode::Constant, &[1]).unwrap(),
                    code::make(Opcode::GreaterThan, &[]).unwrap(),
                    code::make(Opcode::Pop, &[]).unwrap(),
                ],
            ),
            (
                "1 < 2",
                vec![2.into(), 1.into()],
                vec![
                    code::make(Opcode::Constant, &[0]).unwrap(),
                    code::make(Opcode::Constant, &[1]).unwrap(),
                    code::make(Opcode::GreaterThan, &[]).unwrap(),
                    code::make(Opcode::Pop, &[]).unwrap(),
                ],
            ),
            (
                "1 == 2",
                vec![1.into(), 2.into()],
                vec![
                    code::make(Opcode::Constant, &[0]).unwrap(),
                    code::make(Opcode::Constant, &[1]).unwrap(),
                    code::make(Opcode::Equal, &[]).unwrap(),
                    code::make(Opcode::Pop, &[]).unwrap(),
                ],
            ),
            (
                "1 != 2",
                vec![1.into(), 2.into()],
                vec![
                    code::make(Opcode::Constant, &[0]).unwrap(),
                    code::make(Opcode::Constant, &[1]).unwrap(),
                    code::make(Opcode::NotEqual, &[]).unwrap(),
                    code::make(Opcode::Pop, &[]).unwrap(),
                ],
            ),
            (
                "true == false",
                vec![],
                vec![
                    code::make(Opcode::True, &[]).unwrap(),
                    code::make(Opcode::False, &[]).unwrap(),
                    code::make(Opcode::Equal, &[]).unwrap(),
                    code::make(Opcode::Pop, &[]).unwrap(),
                ],
            ),
            (
                "true != false",
                vec![],
                vec![
                    code::make(Opcode::True, &[]).unwrap(),
                    code::make(Opcode::False, &[]).unwrap(),
                    code::make(Opcode::NotEqual, &[]).unwrap(),
                    code::make(Opcode::Pop, &[]).unwrap(),
                ],
            ),
            (
                "!true",
                vec![],
                vec![
                    code::make(Opcode::True, &[]).unwrap(),
                    code::make(Opcode::Bang, &[]).unwrap(),
                    code::make(Opcode::Pop, &[]).unwrap(),
                ],
            ),
        ]);
    }

    #[test]
    fn test_conditionals() {
        let cases = vec![
            (
                "if (true) { 10 }; 3333;",
                vec![10.into(), 3333.into()],
                vec![
                    // 0000
                    make_single(Opcode::True),
                    // 0001
                    code::make(Opcode::JumpFalsy, &[10]).unwrap(),
                    // 0004
                    code::make(Opcode::Constant, &[0]).unwrap(),
                    // 0007
                    code::make(Opcode::Jump, &[11]).unwrap(),
                    // 0010
                    make_single(Opcode::Null),
                    // 0011
                    code::make(Opcode::Pop, &[]).unwrap(),
                    // 0012
                    code::make(Opcode::Constant, &[1]).unwrap(),
                    // 0015
                    code::make(Opcode::Pop, &[]).unwrap(),
                ],
            ),
            (
                "if (true) { 10 } else { 20 }; 3333;",
                vec![10.into(), 20.into(), 3333.into()],
                vec![
                    // 0000
                    make_single(Opcode::True),
                    // 0001
                    code::make(Opcode::JumpFalsy, &[10]).unwrap(),
                    // 0004
                    code::make(Opcode::Constant, &[0]).unwrap(),
                    // 0007
                    code::make(Opcode::Jump, &[13]).unwrap(),
                    // 0010
                    code::make(Opcode::Constant, &[1]).unwrap(),
                    // 0013
                    code::make(Opcode::Pop, &[]).unwrap(),
                    // 0008
                    code::make(Opcode::Constant, &[2]).unwrap(),
                    // 0011
                    code::make(Opcode::Pop, &[]).unwrap(),
                ],
            ),
        ];

        run_compiler_tests(cases);
    }

    #[test]
    fn test_global_let_statements() {
        let cases = vec![
            (
                "let one = 1;
let two = 2;",
                vec![1.into(), 2.into()],
                vec![
                    code::make(Opcode::Constant, &[0]).unwrap(),
                    code::make(Opcode::SetGlobal, &[0]).unwrap(),
                    code::make(Opcode::Constant, &[1]).unwrap(),
                    code::make(Opcode::SetGlobal, &[1]).unwrap(),
                ],
            ),
            (
                "let one = 1;
one;",
                vec![1.into()],
                vec![
                    code::make(Opcode::Constant, &[0]).unwrap(),
                    code::make(Opcode::SetGlobal, &[0]).unwrap(),
                    code::make(Opcode::GetGlobal, &[0]).unwrap(),
                    make_single(Opcode::Pop),
                ],
            ),
            (
                "let one = 1;
let two = one;
two;",
                vec![1.into()],
                vec![
                    code::make(Opcode::Constant, &[0]).unwrap(),
                    code::make(Opcode::SetGlobal, &[0]).unwrap(),
                    code::make(Opcode::GetGlobal, &[0]).unwrap(),
                    code::make(Opcode::SetGlobal, &[1]).unwrap(),
                    code::make(Opcode::GetGlobal, &[1]).unwrap(),
                    make_single(Opcode::Pop),
                ],
            ),
        ];

        run_compiler_tests(cases);
    }

    #[test]
    fn test_string_expressions() {
        let cases = vec![
            (
                "\"monkey\"",
                vec!["monkey".into()],
                vec![
                    code::make(Opcode::Constant, &[0]).unwrap(),
                    make_single(Opcode::Pop),
                ],
            ),
            (
                "\"mon\" + \"key\"",
                vec!["mon".into(), "key".into()],
                vec![
                    code::make(Opcode::Constant, &[0]).unwrap(),
                    code::make(Opcode::Constant, &[1]).unwrap(),
                    make_single(Opcode::Add),
                    make_single(Opcode::Pop),
                ],
            ),
        ];

        run_compiler_tests(cases);
    }

    #[test]
    fn test_array_literals() {
        let cases = vec![
            (
                "[]",
                vec![],
                vec![
                    code::make(Opcode::Array, &[0]).unwrap(),
                    make_single(Opcode::Pop),
                ],
            ),
            (
                "[1, 2, 3]",
                vec![1.into(), 2.into(), 3.into()],
                vec![
                    code::make(Opcode::Constant, &[0]).unwrap(),
                    code::make(Opcode::Constant, &[1]).unwrap(),
                    code::make(Opcode::Constant, &[2]).unwrap(),
                    code::make(Opcode::Array, &[3]).unwrap(),
                    make_single(Opcode::Pop),
                ],
            ),
            (
                "[1 + 2, 3 - 4, 5 * 6]",
                vec![1.into(), 2.into(), 3.into(), 4.into(), 5.into(), 6.into()],
                vec![
                    code::make(Opcode::Constant, &[0]).unwrap(),
                    code::make(Opcode::Constant, &[1]).unwrap(),
                    make_single(Opcode::Add),
                    code::make(Opcode::Constant, &[2]).unwrap(),
                    code::make(Opcode::Constant, &[3]).unwrap(),
                    make_single(Opcode::Sub),
                    code::make(Opcode::Constant, &[4]).unwrap(),
                    code::make(Opcode::Constant, &[5]).unwrap(),
                    make_single(Opcode::Mul),
                    code::make(Opcode::Array, &[3]).unwrap(),
                    make_single(Opcode::Pop),
                ],
            ),
        ];

        run_compiler_tests(cases);
    }

    fn run_compiler_tests(cases: Vec<(&str, Vec<Object>, Vec<Instructions>)>) {
        for (input, constants, instructions) in cases.into_iter() {
            let program = parse(input);

            let mut compiler = Compiler::default();
            compiler.compile(program.into()).unwrap();

            let bytecode = compiler.bytecode();
            test_instructions(instructions, bytecode.instructions);
            assert_eq!(constants, bytecode.constants);
        }
    }

    fn make_single(opcode: Opcode) -> Instructions {
        code::make(opcode, &[]).unwrap()
    }

    fn parse(input: &str) -> ast::Program {
        Parser::new(Lexer::new(input.to_owned()))
            .parse_program()
            .expect("Parse errors found")
    }

    fn test_instructions(expected: Vec<Instructions>, actual: Instructions) {
        let concatted = concat_instructions(expected);

        if concatted != actual {
            panic!(format!("Expected {}, received {}", concatted, actual));
        }
    }
}
