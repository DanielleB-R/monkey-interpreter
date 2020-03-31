use crate::code::{self, Opcode};
use crate::compiler;
use crate::object::Object;
use std::convert::TryInto;

pub static STACK_SIZE: usize = 2048;

pub struct VM {
    constants: Vec<Object>,
    instructions: code::Instructions,

    stack: Vec<Object>,
    sp: usize,
}

impl VM {
    pub fn new(bytecode: compiler::Bytecode) -> Self {
        Self {
            constants: bytecode.constants,
            instructions: bytecode.instructions,

            stack: vec![Object::Null; STACK_SIZE],
            sp: 0,
        }
    }

    pub fn stack_top(&self) -> Option<&Object> {
        if self.sp == 0 {
            None
        } else {
            Some(&self.stack[self.sp - 1])
        }
    }

    pub fn run(&mut self) -> Result<(), ()> {
        let mut ip = 0;
        while ip < self.instructions.len() {
            let op: Opcode = self.instructions[ip].try_into().unwrap();

            match op {
                Opcode::Constant => {
                    let const_index = code::read_u16(&self.instructions[ip + 1..]);
                    ip += 2;
                    self.push(self.constants[const_index as usize].clone())?;
                }
                Opcode::Add => {
                    let right = self.pop();
                    let left = self.pop();
                    match (left, right) {
                        (Object::Integer(l), Object::Integer(r)) => {
                            self.push(Object::Integer(l + r))?
                        }
                        _ => return Err(()),
                    }
                }
                Opcode::Maximum => panic!("Maximum opcode should not be emitted"),
                _ => panic!("unimplemented"),
            }
            ip += 1;
        }
        Ok(())
    }

    fn push(&mut self, obj: Object) -> Result<(), ()> {
        if self.sp >= STACK_SIZE {
            return Err(()); // stack overflow
        }

        self.stack[self.sp] = obj;
        self.sp += 1;

        Ok(())
    }

    fn pop(&mut self) -> Object {
        let obj = self.stack[self.sp - 1].clone();
        self.sp -= 1;
        obj
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast;
    use crate::compiler::Compiler;
    use crate::lexer::Lexer;
    use crate::object::Object;
    use crate::parser::Parser;

    #[test]
    fn test_integer_arithmetic() {
        let cases = vec![
            ("1", Object::Integer(1)),
            ("2", Object::Integer(2)),
            ("1 + 2", Object::Integer(3)),
        ];

        run_vm_tests(cases);
    }

    fn parse(input: &str) -> ast::Program {
        Parser::new(Lexer::new(input.to_owned()))
            .parse_program()
            .expect("Parse errors found")
    }

    fn run_vm_tests(tests: Vec<(&str, Object)>) {
        for (input, output) in tests {
            let program = parse(input);

            let mut comp = Compiler::default();
            comp.compile(program.into()).unwrap();

            let mut vm = VM::new(comp.bytecode());
            vm.run().unwrap();

            assert_eq!(vm.stack_top().unwrap(), &output);
        }
    }
}
