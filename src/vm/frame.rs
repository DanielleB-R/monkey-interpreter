use crate::code::Instructions;

#[derive(Debug, Clone)]
pub struct Frame {
    func: Instructions,
    ip: usize,
}

impl From<Instructions> for Frame {
    fn from(func: Instructions) -> Self {
        Self { func, ip: 0 }
    }
}

impl Frame {
    pub fn instructions(&self) -> &Instructions {
        &self.func
    }
}
