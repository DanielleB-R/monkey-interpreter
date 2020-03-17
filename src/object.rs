use std::fmt::{self, Display, Formatter};

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    ReturnValue(Box<Object>),
    Integer(i64),
    Boolean(bool),
    Null,
}

impl Display for Object {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::ReturnValue(obj) => write!(f, "{}", obj),
            Self::Integer(n) => write!(f, "{}", n),
            Self::Boolean(b) => write!(f, "{}", b),
            Self::Null => write!(f, "null"),
        }
    }
}

impl Object {
    pub fn is_return_value(&self) -> bool {
        match self {
            Self::ReturnValue(_) => true,
            _ => false,
        }
    }
}
