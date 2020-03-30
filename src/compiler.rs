use crate::ast::Node;
use crate::code::Instructions;
use crate::object::Object;

#[derive(Debug, Clone, Default)]
pub struct Compiler {
    pub instructions: Instructions,
    pub constants: Vec<Object>,
}

impl Compiler {
    pub fn compile(&mut self, node: Node) -> Result<(), ()> {
        Ok(())
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

        assert_eq!(concatted, actual);
    }
}
