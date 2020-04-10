use crate::code::Instructions;

#[derive(Debug, Clone)]
pub struct Frame {
    func: Instructions,
    pub ip: isize,
}

impl From<Instructions> for Frame {
    fn from(func: Instructions) -> Self {
        Self { func, ip: -1 }
    }
}

impl Frame {
    pub fn instructions(&self) -> &Instructions {
        &self.func
    }
}
