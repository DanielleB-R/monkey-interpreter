use super::{EvalError, Object};
use std::convert::TryFrom;
use std::fmt::{self, Display, Formatter};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum HashKey {
    String(String),
    Integer(i64),
    Boolean(bool),
}

impl Display for HashKey {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Integer(n) => write!(f, "{}", n),
            Self::Boolean(b) => write!(f, "{}", b),
            Self::String(s) => write!(f, "{}", s),
        }
    }
}

impl From<i64> for HashKey {
    fn from(n: i64) -> Self {
        Self::Integer(n)
    }
}

impl From<bool> for HashKey {
    fn from(b: bool) -> Self {
        Self::Boolean(b)
    }
}

impl From<String> for HashKey {
    fn from(s: String) -> Self {
        Self::String(s)
    }
}

impl From<&str> for HashKey {
    fn from(s: &str) -> Self {
        s.to_owned().into()
    }
}

impl TryFrom<Object> for HashKey {
    type Error = EvalError;

    fn try_from(obj: Object) -> std::result::Result<Self, Self::Error> {
        match obj {
            Object::String(s) => Ok(Self::String(s)),
            Object::Integer(s) => Ok(Self::Integer(s)),
            Object::Boolean(s) => Ok(Self::Boolean(s)),
            o => Err(EvalError::NotHashable {
                type_name: o.type_name(),
            }),
        }
    }
}

impl TryFrom<&Object> for HashKey {
    type Error = EvalError;

    fn try_from(obj: &Object) -> std::result::Result<Self, Self::Error> {
        HashKey::try_from(obj.clone())
    }
}
