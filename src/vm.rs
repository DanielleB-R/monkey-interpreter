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
                Opcode::Add | Opcode::Sub | Opcode::Mul | Opcode::Div => {
                    self.execute_binary_operation(op)?;
                }
                Opcode::Equal | Opcode::NotEqual | Opcode::GreaterThan => {
                    self.execute_comparison(op)?;
                }
                Opcode::Bang => {
                    self.execute_bang_operator()?;
                }
                Opcode::Minus => {
                    self.execute_minus_operator()?;
                }
                Opcode::Pop => {
                    self.pop();
                }
                Opcode::True => self.push(true.into())?,
                Opcode::False => self.push(false.into())?,
                Opcode::Null => self.push(Object::Null)?,
                Opcode::JumpFalsy => {
                    let pos = code::read_u16(&self.instructions[ip + 1..]);
                    ip += 2;
                    let condition = self.pop();

                    if !condition.truth_value() {
                        ip = (pos - 1) as usize;
                    }
                }
                Opcode::Jump => {
                    let pos = code::read_u16(&self.instructions[ip + 1..]);
                    ip = (pos - 1) as usize;
                }
                Opcode::Maximum => panic!("Maximum opcode should not be emitted"),
                _ => {
                    println!("unimplemented");
                }
            }
            ip += 1;
        }
        Ok(())
    }

    fn execute_comparison(&mut self, op: Opcode) -> Result<(), ()> {
        let right = self.pop();
        let left = self.pop();

        match op {
            Opcode::Equal => self.push((right == left).into()),
            Opcode::NotEqual => self.push((right != left).into()),
            Opcode::GreaterThan => match (left, right) {
                (Object::Integer(l), Object::Integer(r)) => self.push((l > r).into()),
                _ => Err(()),
            },
            _ => Err(()),
        }
    }

    fn execute_bang_operator(&mut self) -> Result<(), ()> {
        let operand = self.pop();
        self.push((!operand.truth_value()).into())
    }

    fn execute_minus_operator(&mut self) -> Result<(), ()> {
        let operand = self.pop();
        match operand {
            Object::Integer(n) => self.push((-n).into()),
            _ => Err(()),
        }
    }

    fn execute_binary_operation(&mut self, op: Opcode) -> Result<(), ()> {
        let right = self.pop();
        let left = self.pop();
        match (left, right) {
            (Object::Integer(l), Object::Integer(r)) => {
                self.execute_binary_integer_operation(op, l, r)
            }
            _ => Err(()),
        }
    }

    fn execute_binary_integer_operation(
        &mut self,
        op: Opcode,
        left: i64,
        right: i64,
    ) -> Result<(), ()> {
        let result = match op {
            Opcode::Add => left + right,
            Opcode::Sub => left - right,
            Opcode::Mul => left * right,
            Opcode::Div => left / right,
            _ => return Err(()),
        };

        self.push(result.into())
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

    pub(crate) fn last_popped_stack_element(&self) -> &Object {
        &self.stack[self.sp]
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
            ("1 - 2", Object::Integer(-1)),
            ("1 * 2", Object::Integer(2)),
            ("4 / 2", Object::Integer(2)),
            ("50 / 2 * 2 + 10 - 5", Object::Integer(55)),
            ("5 + 5 + 5 + 5 - 10", Object::Integer(10)),
            ("2 * 2 * 2 * 2 * 2", Object::Integer(32)),
            ("5 * 2 + 10", 20.into()),
            ("5 + 2 * 10", 25.into()),
            ("5 * (2 + 10)", 60.into()),
            ("-5", (-5).into()),
            ("-10", (-10).into()),
            ("-50 + 100 + -50", 0.into()),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50.into()),
        ];

        run_vm_tests(cases);
    }

    #[test]
    fn test_boolean_expressions() {
        let cases = vec![
            ("true", true.into()),
            ("false", false.into()),
            ("1 < 2", true.into()),
            ("1 > 2", false.into()),
            ("1 < 1", false.into()),
            ("1 > 1", false.into()),
            ("1 == 1", true.into()),
            ("1 != 1", false.into()),
            ("1 == 2", false.into()),
            ("1 != 2", true.into()),
            ("true == true", true.into()),
            ("false == false", true.into()),
            ("true == false", false.into()),
            ("true != false", true.into()),
            ("false != true", true.into()),
            ("(1 < 2) == true", true.into()),
            ("(1 < 2) == false", false.into()),
            ("(1 > 2) == true", false.into()),
            ("(1 > 2) == false", true.into()),
            ("!true", false.into()),
            ("!false", true.into()),
            ("!5", false.into()),
            ("!!true", true.into()),
            ("!!false", false.into()),
            ("!!5", true.into()),
            ("!(if (false) { 5; })", true.into()),
        ];

        run_vm_tests(cases);
    }

    #[test]
    fn test_conditionals() {
        let cases = vec![
            ("if (true) { 10 }", 10.into()),
            ("if (true) { 10 } else { 20 }", 10.into()),
            ("if (false) { 10 } else { 20 }", 20.into()),
            ("if (1) { 10 }", 10.into()),
            ("if (1 < 2) { 10 }", 10.into()),
            ("if (1 < 2) { 10 } else { 20 }", 10.into()),
            ("if (1 > 2) { 10 } else { 20 }", 20.into()),
            ("if (1 > 2) { 10 }", Object::Null),
            ("if (false) { 10 }", Object::Null),
            ("if ((if (false) { 10 })) { 10 } else { 20 }", 20.into()),
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

            assert_eq!(vm.last_popped_stack_element(), &output);
        }
    }
}
