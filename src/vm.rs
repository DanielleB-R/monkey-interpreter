use crate::code::{self, Opcode};
use crate::compiler;
use crate::object::{EvalError, HashValue, Object};
use custom_error::custom_error;
use std::convert::TryInto;

custom_error! {
    pub VMError

    UnknownOperator{op: Opcode, left: &'static str, right: &'static str} = "unknown operator: {op} ({left} {right})",
    StackOverflow = "stack overflow",
    UnknownStringOperator{op: Opcode} = "unknown string operator: {op}",
    UnknownIntegerOperator{op: Opcode} = "unknown integer operator: {op}",
    UnsupportedBinaryTypes{left: &'static str, right: &'static str} = "unsupported types for binary operation: {left} {right}",
    UnsupportedNegation{type_name: &'static str} = "unsupported type for negation: {type_name}",
    ErrorEval{source: EvalError} = "{source}",
}

pub static STACK_SIZE: usize = 2048;
pub static GLOBALS_SIZE: usize = 65536;

pub struct VM {
    constants: Vec<Object>,
    instructions: code::Instructions,

    stack: Vec<Object>,
    sp: usize,

    globals: Vec<Object>,
}

impl VM {
    pub fn new(bytecode: compiler::Bytecode) -> Self {
        Self {
            constants: bytecode.constants,
            instructions: bytecode.instructions,

            stack: vec![Object::Null; STACK_SIZE],
            sp: 0,

            globals: vec![Object::Null; GLOBALS_SIZE],
        }
    }

    pub fn with_state(bytecode: compiler::Bytecode, state: Vec<Object>) -> Self {
        Self {
            constants: bytecode.constants,
            instructions: bytecode.instructions,

            stack: vec![Object::Null; STACK_SIZE],
            sp: 0,

            globals: state,
        }
    }

    pub fn stack_top(&self) -> Option<&Object> {
        if self.sp == 0 {
            None
        } else {
            Some(&self.stack[self.sp - 1])
        }
    }

    pub fn run(&mut self) -> Result<(), VMError> {
        let mut ip = 0;
        while ip < self.instructions.len() {
            let op: Opcode = self.instructions[ip].try_into().unwrap();

            match op {
                Opcode::Constant => {
                    let const_index = self.get_u16_arg(&mut ip);
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
                    let target = self.get_u16_arg(&mut ip);
                    let condition = self.pop();

                    if !condition.truth_value() {
                        ip = (target - 1) as usize;
                    }
                }
                Opcode::Jump => {
                    let target = self.get_u16_arg(&mut ip);
                    ip = (target - 1) as usize;
                }
                Opcode::SetGlobal => {
                    let index = self.get_u16_arg(&mut ip);

                    self.globals[index as usize] = self.pop();
                }
                Opcode::GetGlobal => {
                    let pos = self.get_u16_arg(&mut ip);

                    self.push(self.globals[pos as usize].clone())?
                }
                Opcode::Array => {
                    let len = self.get_u16_arg(&mut ip) as usize;

                    let arr = self.build_array(self.sp - len, self.sp);
                    self.sp -= len;
                    self.push(arr)?;
                }
                Opcode::Hash => {
                    let len = self.get_u16_arg(&mut ip) as usize;

                    let hash = self.build_hash(self.sp - len, self.sp)?;
                    self.sp -= len;
                    self.push(hash)?;
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

    fn get_u16_arg(&mut self, ip: &mut usize) -> u16 {
        let arg = code::read_u16(&self.instructions[*ip + 1..]);
        *ip += 2;
        arg
    }

    fn execute_comparison(&mut self, op: Opcode) -> Result<(), VMError> {
        let right = self.pop();
        let left = self.pop();

        match op {
            Opcode::Equal => self.push((right == left).into()),
            Opcode::NotEqual => self.push((right != left).into()),
            Opcode::GreaterThan => match (left, right) {
                (Object::Integer(l), Object::Integer(r)) => self.push((l > r).into()),
                (l, r) => Err(VMError::UnknownOperator {
                    op,
                    left: l.type_name(),
                    right: r.type_name(),
                }),
            },
            o => Err(VMError::UnknownOperator {
                op: o,
                left: left.type_name(),
                right: right.type_name(),
            }),
        }
    }

    fn execute_bang_operator(&mut self) -> Result<(), VMError> {
        let operand = self.pop();
        self.push((!operand.truth_value()).into())
    }

    fn execute_minus_operator(&mut self) -> Result<(), VMError> {
        let operand = self.pop();
        match operand {
            Object::Integer(n) => self.push((-n).into()),
            o => Err(VMError::UnsupportedNegation {
                type_name: o.type_name(),
            }),
        }
    }

    fn execute_binary_operation(&mut self, op: Opcode) -> Result<(), VMError> {
        let right = self.pop();
        let left = self.pop();
        match (left, right) {
            (Object::Integer(l), Object::Integer(r)) => {
                self.execute_binary_integer_operation(op, l, r)
            }
            (Object::String(l), Object::String(r)) => {
                self.execute_binary_string_operation(op, l, r)
            }
            (l, r) => Err(VMError::UnsupportedBinaryTypes {
                left: l.type_name(),
                right: r.type_name(),
            }),
        }
    }

    fn execute_binary_integer_operation(
        &mut self,
        op: Opcode,
        left: i64,
        right: i64,
    ) -> Result<(), VMError> {
        let result = match op {
            Opcode::Add => left + right,
            Opcode::Sub => left - right,
            Opcode::Mul => left * right,
            Opcode::Div => left / right,
            _ => return Err(VMError::UnknownIntegerOperator { op }),
        };

        self.push(result.into())
    }

    fn execute_binary_string_operation(
        &mut self,
        op: Opcode,
        left: String,
        right: String,
    ) -> Result<(), VMError> {
        if op == Opcode::Add {
            self.push((left + &right).into())
        } else {
            Err(VMError::UnknownStringOperator { op })
        }
    }

    fn build_array(&self, start_index: usize, end_index: usize) -> Object {
        self.stack[start_index..end_index]
            .iter()
            .cloned()
            .collect::<Vec<Object>>()
            .into()
    }

    fn build_hash(&self, start_index: usize, end_index: usize) -> Result<Object, VMError> {
        let mut hash: HashValue = Default::default();
        for pair in self.stack[start_index..end_index].chunks(2) {
            hash.values
                .insert(pair[0].clone().try_into()?, pair[1].clone().into());
        }
        Ok(hash.into())
    }

    fn push(&mut self, obj: Object) -> Result<(), VMError> {
        if self.sp >= STACK_SIZE {
            return Err(VMError::StackOverflow); // stack overflow
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

    pub fn into_state(self) -> Vec<Object> {
        self.globals
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

    #[test]
    fn test_global_let_statments() {
        let cases = vec![
            ("let one = 1; one", 1.into()),
            ("let one = 1; let two = 2; one + two", 3.into()),
            ("let one = 1; let two = one + one; one + two", 3.into()),
        ];

        run_vm_tests(cases);
    }

    #[test]
    fn test_string_expressions() {
        let cases = vec![
            ("\"monkey\"", "monkey".into()),
            ("\"mon\" + \"key\"", "monkey".into()),
            ("\"mon\" + \"key\" + \"banana\"", "monkeybanana".into()),
        ];

        run_vm_tests(cases);
    }

    #[test]
    fn test_array_expressions() {
        let cases = vec![
            ("[]", vec![].into()),
            ("[1, 2, 3]", vec![1.into(), 2.into(), 3.into()].into()),
            (
                "[1 + 2, 3 * 4, 5 + 6]",
                vec![3.into(), 12.into(), 11.into()].into(),
            ),
        ];

        run_vm_tests(cases);
    }

    #[test]
    fn test_hash_literals() {
        let cases = vec![
            ("{}", HashValue::default().into()),
            (
                "{1: 2, 2: 3}",
                vec![(1.into(), 2.into()), (2.into(), 3.into())]
                    .into_iter()
                    .collect::<HashValue>()
                    .into(),
            ),
            (
                "{1 + 1: 2 * 2, 3 + 3: 4 * 4}",
                vec![(2.into(), 4.into()), (6.into(), 16.into())]
                    .into_iter()
                    .collect::<HashValue>()
                    .into(),
            ),
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
