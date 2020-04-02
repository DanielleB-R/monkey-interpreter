use crate::ast::{Expression, Node, Operator, Statement};
use crate::code::{self, BytecodeError, Instructions, Opcode};
use crate::object::Object;
use custom_error::custom_error;

custom_error! {
    pub CompileError

    InvalidBytecode{source: BytecodeError} = "invalid bytecode: {source}",
    UnknownOperator{op: Operator} = "unknown operator {op}",
}

#[derive(Debug, Clone, Default)]
pub struct Compiler {
    pub instructions: Instructions,
    pub constants: Vec<Object>,
}

impl Compiler {
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
                _ => panic!("unimplemented"),
            },
            Node::Expression(expr) => match expr {
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
                Expression::Boolean(b) => {
                    if b.value {
                        self.emit(Opcode::True, &[]);
                    } else {
                        self.emit(Opcode::False, &[]);
                    }
                }
                _ => panic!("unimplemented"),
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
        Some(self.add_instruction(ins))
    }

    pub fn add_instruction(&mut self, ins: Instructions) -> usize {
        let pos = self.instructions.len();
        self.instructions.append(ins);
        pos
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
