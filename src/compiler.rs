use crate::ast::{Expression, Node, Statement};
use crate::code::{self, BytecodeError, Instructions, Opcode};
use crate::object::Object;

#[derive(Debug, Clone, Default)]
pub struct Compiler {
    pub instructions: Instructions,
    pub constants: Vec<Object>,
}

impl Compiler {
    pub fn compile(&mut self, node: Node) -> Result<(), BytecodeError> {
        match node {
            Node::Program(p) => {
                for stmt in p.statements {
                    self.compile(stmt.into())?;
                }
            }
            Node::Statement(stmt) => match stmt {
                Statement::Expr(e) => self.compile(e.expression.into())?,
                _ => panic!("unimplemented"),
            },
            Node::Expression(expr) => match expr {
                Expression::Infix(infix) => {
                    self.compile((*infix.left).into())?;
                    self.compile((*infix.right).into())?;
                }
                Expression::IntegerLiteral(int) => {
                    let constant = self.add_constant(Object::Integer(int.value));
                    self.emit(Opcode::Constant, &[constant]);
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
        let cases = vec![(
            "1 + 2",
            vec![Object::Integer(1), Object::Integer(2)],
            vec![
                code::make(Opcode::Constant, &[0]).unwrap(),
                code::make(Opcode::Constant, &[1]).unwrap(),
            ],
        )];

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
