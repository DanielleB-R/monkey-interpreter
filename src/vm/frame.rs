use crate::code::Instructions;
use crate::object::{Closure, CompiledFunction, Object};
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Frame {
    pub func: Rc<Closure>,
    pub ip: isize,
    pub base_pointer: isize,
}

impl From<CompiledFunction> for Frame {
    fn from(func: CompiledFunction) -> Self {
        Self::new(Rc::new(func.into()), 0)
    }
}

impl From<Instructions> for Frame {
    fn from(func: Instructions) -> Self {
        CompiledFunction::new(func, 0, 0).into()
    }
}

impl Frame {
    pub fn new(func: Rc<Closure>, base_pointer: isize) -> Self {
        Self {
            func,
            ip: -1,
            base_pointer,
        }
    }

    pub fn instructions(&self) -> &Instructions {
        &self.func.func.instructions
    }

    pub fn get_free(&self, index: usize) -> Rc<Object> {
        Rc::clone(&self.func.free[index])
    }

    pub fn is_valid_ip(&self) -> bool {
        self.ip < ((self.func.func.instructions.len() - 1) as isize)
    }
}
