use crate::code::Instructions;
use crate::object::{Closure, CompiledFunction};

#[derive(Debug, Clone)]
pub struct Frame {
    func: Closure,
    pub ip: isize,
    pub base_pointer: isize,
}

impl From<CompiledFunction> for Frame {
    fn from(func: CompiledFunction) -> Self {
        Self::new(func.into(), 0)
    }
}

impl From<Instructions> for Frame {
    fn from(func: Instructions) -> Self {
        CompiledFunction::new(func, 0, 0).into()
    }
}

impl Frame {
    pub fn new(func: Closure, base_pointer: isize) -> Self {
        Self {
            func,
            ip: -1,
            base_pointer,
        }
    }

    pub fn instructions(&self) -> &Instructions {
        &self.func.func.instructions
    }
}
