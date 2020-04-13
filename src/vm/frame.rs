use crate::code::Instructions;

#[derive(Debug, Clone)]
pub struct Frame {
    func: Instructions,
    pub ip: isize,
    pub base_pointer: isize,
}

impl From<Instructions> for Frame {
    fn from(func: Instructions) -> Self {
        Self::new(func, 0)
    }
}

impl Frame {
    pub fn new(func: Instructions, base_pointer: isize) -> Self {
        Self {
            func,
            ip: -1,
            base_pointer,
        }
    }

    pub fn instructions(&self) -> &Instructions {
        &self.func
    }
}
