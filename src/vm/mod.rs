mod frame;
#[cfg(test)]
mod test;

use crate::builtins::Builtin;
use crate::code::{self, Opcode};
use crate::compiler;
use crate::object::{Closure, EvalError, HashValue, Object};
use custom_error::custom_error;
use frame::Frame;
use std::convert::TryInto;
use std::rc::Rc;

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
    NotAFunction = "not a function",
}

pub static STACK_SIZE: usize = 2048;
pub static GLOBALS_SIZE: usize = 65536;

fn lower_vec(input: Vec<Rc<Object>>) -> Vec<Object> {
    input.into_iter().map(|o| o.as_ref().clone()).collect()
}

pub struct VM {
    constants: Vec<Rc<Object>>,

    stack: Vec<Rc<Object>>,
    sp: usize,

    globals: Vec<Rc<Object>>,

    frames: Vec<Frame>,
    frames_index: usize,
}

impl VM {
    pub fn new(bytecode: compiler::Bytecode) -> Self {
        Self {
            constants: bytecode.constants,

            stack: vec![Rc::new(Object::Null); STACK_SIZE],
            sp: 0,

            globals: vec![Rc::new(Object::Null); GLOBALS_SIZE],

            frames: vec![bytecode.instructions.into()],
            frames_index: 1,
        }
    }

    pub fn with_state(bytecode: compiler::Bytecode, state: Vec<Rc<Object>>) -> Self {
        Self {
            constants: bytecode.constants,

            stack: vec![Rc::new(Object::Null); STACK_SIZE],
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

    pub fn run(&mut self) -> Result<(), VMError> {
        let mut ip;
        while self.current_frame().is_valid_ip() {
            self.current_frame().ip += 1;

            ip = self.current_frame().ip as usize;
            let op: Opcode = self.current_frame().instructions()[ip].try_into().unwrap();

            match op {
                Opcode::Constant => {
                    let const_index = self.get_u16_arg(ip);
                    self.push(Rc::clone(&self.constants[const_index as usize]))?;
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
                Opcode::True => self.push(Rc::new(true.into()))?,
                Opcode::False => self.push(Rc::new(false.into()))?,
                Opcode::Null => self.push(Rc::new(Object::Null))?,
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

                    self.globals[index as usize] = self.pop()
                }
                Opcode::GetGlobal => {
                    let pos = self.get_u16_arg(ip);

                    self.push(Rc::clone(&self.globals[pos as usize]))?
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

                    self.push(Rc::clone(&self.stack[stack_index]))?;
                }
                Opcode::GetBuiltin => {
                    let index = self.get_u8_arg(ip);

                    let builtin = Object::Builtin(unsafe { std::mem::transmute(index) });
                    self.push(Rc::new(builtin))?;
                }
                Opcode::GetFree => {
                    let free_index = self.get_u8_arg(ip) as usize;

                    let value = self.current_frame().get_free(free_index);
                    self.push(Rc::new(value))?;
                }
                Opcode::Array => {
                    let len = self.get_u16_arg(ip) as usize;

                    let arr = self.build_array(self.sp - len, self.sp);
                    self.sp -= len;
                    self.push(Rc::new(arr))?;
                }
                Opcode::Hash => {
                    let len = self.get_u16_arg(ip) as usize;

                    let hash = self.build_hash(self.sp - len, self.sp)?;
                    self.sp -= len;
                    self.push(Rc::new(hash))?;
                }
                Opcode::Call => {
                    let num_args = self.get_u8_arg(ip) as usize;

                    self.execute_call(num_args)?;
                }
                Opcode::Closure => {
                    let const_index = self.get_u16_arg(ip) as usize;
                    let num_free = self.get_u8_arg(ip + 2) as usize;

                    self.push_closure(const_index, num_free)?;
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

                    self.push(Rc::new(Object::Null))?;
                }
                Opcode::CurrentClosure => {
                    let closure = self.current_frame().func.clone().into();
                    self.push(Rc::new(closure))?;
                }
                Opcode::Maximum => panic!("Maximum opcode should not be emitted"),
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

    fn execute_call(&mut self, num_args: usize) -> Result<(), VMError> {
        let callee = Rc::clone(&self.stack[self.sp - 1 - num_args]);
        match callee.as_ref() {
            Object::Closure(c) => self.call_closure(c, num_args),
            Object::Builtin(f) => self.call_builtin(f, num_args),
            _ => Err(VMError::Uncallable),
        }
    }

    fn call_closure(&mut self, cf: &Closure, num_args: usize) -> Result<(), VMError> {
        if num_args != cf.func.num_parameters {
            return Err(VMError::WrongArgumentCount {
                expected: cf.func.num_parameters,
                found: num_args,
            });
        }
        let num_locals = cf.func.num_locals as usize;
        self.push_frame(Frame::new(cf.clone(), (self.sp - num_args) as isize));
        self.sp += num_locals;
        Ok(())
    }

    fn call_builtin(&mut self, f: &Builtin, num_args: usize) -> Result<(), VMError> {
        let args = self.stack[self.sp - num_args..self.sp].to_vec();

        self.push(Rc::new(f.func()(lower_vec(args))?))
    }

    fn push_closure(&mut self, const_index: usize, num_free: usize) -> Result<(), VMError> {
        let constant = &self.constants[const_index];
        let mut closure: Closure = match constant.as_ref() {
            Object::CompiledFunction(cf) => cf.clone().into(),
            _ => return Err(VMError::NotAFunction),
        };

        for i in 0..num_free {
            closure
                .free
                .push(self.stack[self.sp - num_free + i].as_ref().clone());
        }
        self.sp -= num_free;

        self.push(Rc::new(closure.into()))
    }

    fn execute_comparison(&mut self, op: Opcode) -> Result<(), VMError> {
        let right = self.pop();
        let left = self.pop();

        match op {
            Opcode::Equal => self.push(Rc::new((right == left).into())),
            Opcode::NotEqual => self.push(Rc::new((right != left).into())),
            Opcode::GreaterThan => match (left.as_ref(), right.as_ref()) {
                (Object::Integer(l), Object::Integer(r)) => self.push(Rc::new((l > r).into())),
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
        self.push(Rc::new((!operand.truth_value()).into()))
    }

    fn execute_minus_operator(&mut self) -> Result<(), VMError> {
        let operand = self.pop();
        match operand.as_ref() {
            Object::Integer(n) => self.push(Rc::new((-n).into())),
            o => Err(VMError::UnsupportedNegation {
                type_name: o.type_name(),
            }),
        }
    }

    fn execute_binary_operation(&mut self, op: Opcode) -> Result<(), VMError> {
        let right = self.pop();
        let left = self.pop();
        match (left.as_ref(), right.as_ref()) {
            (Object::Integer(l), Object::Integer(r)) => {
                self.execute_binary_integer_operation(op, *l, *r)
            }
            (Object::String(l), Object::String(r)) => {
                self.execute_binary_string_operation(op, l.clone(), r)
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

        self.push(Rc::new(result.into()))
    }

    fn execute_binary_string_operation(
        &mut self,
        op: Opcode,
        left: String,
        right: &str,
    ) -> Result<(), VMError> {
        if op == Opcode::Add {
            self.push(Rc::new((left + right).into()))
        } else {
            Err(VMError::UnknownStringOperator { op })
        }
    }

    fn build_array(&self, start_index: usize, end_index: usize) -> Object {
        lower_vec(self.stack[start_index..end_index].to_vec()).into()
    }

    fn build_hash(&self, start_index: usize, end_index: usize) -> Result<Object, VMError> {
        let mut hash: HashValue = Default::default();
        for pair in self.stack[start_index..end_index].chunks(2) {
            hash.values.insert(
                pair[0].as_ref().clone().try_into()?,
                pair[1].as_ref().clone(),
            );
        }
        Ok(hash.into())
    }

    fn execute_index_expression(
        &mut self,
        left: Rc<Object>,
        index: Rc<Object>,
    ) -> Result<(), VMError> {
        match (left.as_ref(), index.as_ref()) {
            (Object::Array(arr), Object::Integer(int)) => self.execute_array_index(arr, *int),
            (Object::Hash(hash), ind) => self.execute_hash_index(hash, ind),
            (obj, _) => Err(VMError::Unindexable {
                type_name: obj.type_name(),
            }),
        }
    }

    fn execute_array_index(&mut self, left: &[Object], index: i64) -> Result<(), VMError> {
        self.push(Rc::new(
            left.iter()
                .nth(index as usize)
                .cloned()
                .unwrap_or(Object::Null),
        ))
    }

    fn execute_hash_index(&mut self, left: &HashValue, index: &Object) -> Result<(), VMError> {
        self.push(Rc::new(
            left.values
                .get(&index.try_into()?)
                .cloned()
                .unwrap_or(Object::Null),
        ))
    }

    fn push(&mut self, obj: Rc<Object>) -> Result<(), VMError> {
        if self.sp >= STACK_SIZE {
            return Err(VMError::StackOverflow); // stack overflow
        }

        self.stack[self.sp] = obj;
        self.sp += 1;

        Ok(())
    }

    fn pop(&mut self) -> Rc<Object> {
        let obj = &self.stack[self.sp - 1];
        self.sp -= 1;
        Rc::clone(obj)
    }

    pub fn last_popped_stack_element(&self) -> &Object {
        &self.stack[self.sp]
    }

    pub fn into_state(self) -> Vec<Rc<Object>> {
        self.globals
    }
}
