mod frame;

use crate::code::{self, Opcode};
use crate::compiler;
use crate::object::{EvalError, HashValue, Object};
use custom_error::custom_error;
use frame::Frame;
use std::convert::TryInto;

custom_error! {
    #[derive(PartialEq)]
    pub VMError

    UnknownOperator{op: Opcode, left: &'static str, right: &'static str} = "unknown operator: {op} ({left} {right})",
    StackOverflow = "stack overflow",
    UnknownStringOperator{op: Opcode} = "unknown string operator: {op}",
    UnknownIntegerOperator{op: Opcode} = "unknown integer operator: {op}",
    UnsupportedBinaryTypes{left: &'static str, right: &'static str} = "unsupported types for binary operation: {left} {right}",
    UnsupportedNegation{type_name: &'static str} = "unsupported type for negation: {type_name}",
    ErrorEval{source: EvalError} = "{source}",
    Unindexable{type_name: &'static str} = "index operator not supported: {type_name}",
    Uncallable = "calling non-function",
    WrongArgumentCount{expected: usize, found: usize} = "wrong number of arguments: want={expected}, got={found}",
}

pub static STACK_SIZE: usize = 2048;
pub static GLOBALS_SIZE: usize = 65536;

pub struct VM {
    constants: Vec<Object>,

    stack: Vec<Object>,
    sp: usize,

    globals: Vec<Object>,

    frames: Vec<Frame>,
    frames_index: usize,
}

impl VM {
    pub fn new(bytecode: compiler::Bytecode) -> Self {
        Self {
            constants: bytecode.constants,

            stack: vec![Object::Null; STACK_SIZE],
            sp: 0,

            globals: vec![Object::Null; GLOBALS_SIZE],

            frames: vec![bytecode.instructions.into()],
            frames_index: 1,
        }
    }

    pub fn with_state(bytecode: compiler::Bytecode, state: Vec<Object>) -> Self {
        Self {
            constants: bytecode.constants,

            stack: vec![Object::Null; STACK_SIZE],
            sp: 0,

            globals: state,
            frames: vec![bytecode.instructions.into()],
            frames_index: 1,
        }
    }

    fn current_frame(&mut self) -> &mut Frame {
        &mut self.frames[self.frames_index - 1]
    }

    fn push_frame(&mut self, frame: Frame) {
        self.frames.push(frame);
        self.frames_index += 1;
    }

    fn pop_frame(&mut self) -> Frame {
        self.frames_index -= 1;
        self.frames.pop().unwrap()
    }

    pub fn stack_top(&self) -> Option<&Object> {
        if self.sp == 0 {
            None
        } else {
            Some(&self.stack[self.sp - 1])
        }
    }

    pub fn run(&mut self) -> Result<(), VMError> {
        let mut ip;
        while self.current_frame().ip < ((self.current_frame().instructions().len()) - 1) as isize {
            self.current_frame().ip += 1;

            ip = self.current_frame().ip as usize;
            let op: Opcode = self.current_frame().instructions()[ip].try_into().unwrap();

            match op {
                Opcode::Constant => {
                    let const_index = self.get_u16_arg(ip);
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
                Opcode::Index => {
                    let index = self.pop();
                    let left = self.pop();

                    self.execute_index_expression(left, index)?;
                }
                Opcode::Pop => {
                    self.pop();
                }
                Opcode::True => self.push(true.into())?,
                Opcode::False => self.push(false.into())?,
                Opcode::Null => self.push(Object::Null)?,
                Opcode::JumpFalsy => {
                    let target = self.get_u16_arg(ip);
                    let condition = self.pop();

                    if !condition.truth_value() {
                        self.current_frame().ip = (target - 1) as isize;
                    }
                }
                Opcode::Jump => {
                    let target = self.get_u16_arg(ip);
                    self.current_frame().ip = (target - 1) as isize;
                }
                Opcode::SetGlobal => {
                    let index = self.get_u16_arg(ip);

                    self.globals[index as usize] = self.pop();
                }
                Opcode::GetGlobal => {
                    let pos = self.get_u16_arg(ip);

                    self.push(self.globals[pos as usize].clone())?
                }
                Opcode::SetLocal => {
                    let local_index = self.get_u8_arg(ip) as usize;
                    let frame = self.current_frame();
                    let stack_index = (frame.base_pointer as usize) + local_index;
                    self.stack[stack_index] = self.pop();
                }
                Opcode::GetLocal => {
                    let local_index = self.get_u8_arg(ip) as usize;
                    let frame = self.current_frame();
                    let stack_index = (frame.base_pointer as usize) + local_index;

                    self.push(self.stack[stack_index].clone())?;
                }
                Opcode::Array => {
                    let len = self.get_u16_arg(ip) as usize;

                    let arr = self.build_array(self.sp - len, self.sp);
                    self.sp -= len;
                    self.push(arr)?;
                }
                Opcode::Hash => {
                    let len = self.get_u16_arg(ip) as usize;

                    let hash = self.build_hash(self.sp - len, self.sp)?;
                    self.sp -= len;
                    self.push(hash)?;
                }
                Opcode::Call => {
                    let num_args = self.get_u8_arg(ip) as usize;

                    self.call_function(num_args)?;
                }
                Opcode::ReturnValue => {
                    let return_value = self.pop();

                    let frame = self.pop_frame();
                    self.sp = (frame.base_pointer - 1) as usize;

                    self.push(return_value)?;
                }
                Opcode::Return => {
                    let frame = self.pop_frame();
                    self.sp = (frame.base_pointer - 1) as usize;

                    self.push(Object::Null)?;
                }
                Opcode::Maximum => panic!("Maximum opcode should not be emitted"),
                _ => {
                    println!("unimplemented");
                }
            }
        }
        Ok(())
    }

    fn get_u16_arg(&mut self, ip: usize) -> u16 {
        let frame = self.current_frame();
        let arg = code::read_u16(&frame.instructions()[ip + 1..]);
        frame.ip += 2;
        arg
    }

    fn get_u8_arg(&mut self, ip: usize) -> u8 {
        let frame = self.current_frame();
        let arg = frame.instructions()[ip + 1];
        frame.ip += 1;
        arg
    }

    fn call_function(&mut self, num_args: usize) -> Result<(), VMError> {
        let top = self.stack[self.sp - 1 - num_args].clone();
        match top {
            Object::CompiledFunction(cf) => {
                if num_args != cf.num_parameters {
                    return Err(VMError::WrongArgumentCount {
                        expected: cf.num_parameters,
                        found: num_args,
                    });
                }
                self.push_frame(Frame::new(cf.instructions, (self.sp - num_args) as isize));
                self.sp += cf.num_locals as usize;
            }
            _ => return Err(VMError::Uncallable),
        };
        Ok(())
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

    fn execute_index_expression(&mut self, left: Object, index: Object) -> Result<(), VMError> {
        match (left, index) {
            (Object::Array(arr), Object::Integer(int)) => self.execute_array_index(arr, int),
            (Object::Hash(hash), ind) => self.execute_hash_index(hash, ind),
            (obj, _) => Err(VMError::Unindexable {
                type_name: obj.type_name(),
            }),
        }
    }

    fn execute_array_index(&mut self, left: Vec<Object>, index: i64) -> Result<(), VMError> {
        self.push(left.into_iter().nth(index as usize).unwrap_or(Object::Null))
    }

    fn execute_hash_index(&mut self, mut left: HashValue, index: Object) -> Result<(), VMError> {
        self.push(
            left.values
                .remove(&index.try_into()?)
                .unwrap_or(Object::Null),
        )
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

    #[test]
    fn test_index_expressions() {
        let cases = vec![
            ("[1, 2, 3][1]", 2.into()),
            ("[1, 2, 3][0 + 2]", 3.into()),
            ("[[1, 1, 1]][0][0]", 1.into()),
            ("[][0]", Object::Null),
            ("[1, 2, 3][99]", Object::Null),
            ("[1][-1]", Object::Null),
            ("{1: 1, 2: 2}[2]", 2.into()),
            ("{1: 1}[0]", Object::Null),
            ("{}[0]", Object::Null),
        ];

        run_vm_tests(cases);
    }

    #[test]
    fn test_calling_functions_without_arguments() {
        let cases = vec![
            (
                "
let fivePlusTen = fn() { 5 + 10; };
fivePlusTen();
",
                15.into(),
            ),
            (
                "
let one = fn() { 1; };
let two = fn() { 2; };
one() + two()",
                3.into(),
            ),
            (
                "
let a = fn() { 1 };
let b = fn() { a() + 1 };
let c = fn() { b() + 1 };
c();",
                3.into(),
            ),
            (
                "
let earlyExit = fn() { return 99; 100; };
earlyExit();",
                99.into(),
            ),
            (
                "
let earlyExit = fn() { return 99; return 100; };
earlyExit();",
                99.into(),
            ),
            (
                "
let noReturn = fn() { };
noReturn()",
                Object::Null,
            ),
            (
                "
let noReturn = fn() { };
let noReturnTwo = fn() { noReturn(); };
noReturn();
noReturnTwo();",
                Object::Null,
            ),
        ];

        run_vm_tests(cases);
    }

    #[test]
    fn test_first_class_functions() {
        let cases = vec![
            (
                "let returnsOne = fn() { 1; };
let returnsOneReturner = fn() { returnsOne; };
returnsOneReturner()();",
                1.into(),
            ),
            (
                "let returnsOneReturner = fn() {
               let returnsOne = fn() { 1; };
               returnsOne;
           };
           returnsOneReturner()();",
                1.into(),
            ),
        ];

        run_vm_tests(cases)
    }

    #[test]
    fn test_calling_functions_with_bindings() {
        let cases = vec![
            (
                "let one = fn() { let one = 1; one };
one();",
                1.into(),
            ),
            (
                "let oneAndTwo = fn() { let one = 1; let two = 2; one + two; };
           oneAndTwo();",
                3.into(),
            ),
            (
                "let oneAndTwo = fn() { let one = 1; let two = 2; one + two; };
           let threeAndFour = fn() { let three = 3; let four = 4; three + four; };
           oneAndTwo() + threeAndFour();",
                10.into(),
            ),
            (
                "let firstFoobar = fn() { let foobar = 50; foobar; };
           let secondFoobar = fn() { let foobar = 100; foobar; };
           firstFoobar() + secondFoobar();",
                150.into(),
            ),
            (
                "let globalSeed = 50;
           let minusOne = fn() {
let num = 1;
               globalSeed - num;
           }
           let minusTwo = fn() {
               let num = 2;
               globalSeed - num;
           }
           minusOne() + minusTwo();",
                97.into(),
            ),
        ];

        run_vm_tests(cases)
    }

    #[test]
    fn test_calling_functions_with_arguments_and_bindings() {
        let cases = vec![
            (
                "let identity = fn(a) { a; };
           identity(4);",
                4.into(),
            ),
            (
                "let sum = fn(a, b) { a + b; };
           sum(1, 2);",
                3.into(),
            ),
            (
                "let sum = fn(a, b) {
               let c = a + b;
  c;
};
sum(1, 2);",
                3.into(),
            ),
            (
                "let sum = fn(a, b) {
               let c = a + b;
c; };
           sum(1, 2) + sum(3, 4);",
                10.into(),
            ),
            (
                "let sum = fn(a, b) {
let c = a + b;
c; };
           let outer = fn() {
               sum(1, 2) + sum(3, 4);
           };
           outer();",
                10.into(),
            ),
            (
                "let globalNum = 10;
           let sum = fn(a, b) {
               let c = a + b;
               c + globalNum;
};
           let outer = fn() {
               sum(1, 2) + sum(3, 4) + globalNum;
};
           outer() + globalNum;",
                50.into(),
            ),
        ];

        run_vm_tests(cases)
    }

    #[test]
    fn test_calling_functions_with_wrong_arguments() {
        let cases = vec![
            (
                "fn() { 1; }(1);",
                VMError::WrongArgumentCount {
                    expected: 0,
                    found: 1,
                },
            ),
            (
                "fn(a) { a; }();",
                VMError::WrongArgumentCount {
                    expected: 1,
                    found: 0,
                },
            ),
            (
                "fn(a, b) { a + b; }(1);",
                VMError::WrongArgumentCount {
                    expected: 2,
                    found: 1,
                },
            ),
        ];

        for (input, err) in cases {
            let program = parse(input);

            let mut comp = Compiler::default();
            comp.compile(program.into()).unwrap();

            let mut vm = VM::new(comp.bytecode());
            assert_eq!(vm.run().unwrap_err(), err);
        }
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
