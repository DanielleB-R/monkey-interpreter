use std::fmt::{self, Display, Formatter};

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    ReturnValue(Box<Object>),
    // TODO: Make this a Rust error and use Results
    Error(String),
    Integer(i64),
    Boolean(bool),
    Null,
}

impl Display for Object {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::ReturnValue(obj) => write!(f, "{}", obj),
            Self::Error(e) => write!(f, "ERROR: {}", e),
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

    pub fn is_error(&self) -> bool {
        match self {
            Self::Error(_) => true,
            _ => false,
        }
    }

    pub fn type_name(&self) -> &str {
        match self {
            Self::ReturnValue(o) => o.type_name(),
            Self::Error(_) => "ERROR",
            Self::Boolean(_) => "BOOLEAN",
            Self::Integer(_) => "INTEGER",
            Self::Null => "NULL",
        }
    }
}
