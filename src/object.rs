use crate::ast;
use crate::environment::Environment;
use custom_error::custom_error;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::fmt::{self, Display, Formatter};
use std::iter::FromIterator;

custom_error! {
    #[derive(Clone, PartialEq)]
    pub EvalError

    IdentifierNotFound{id: String} = "identifier not found: {id}",
    UnknownPrefixOperator{operator: ast::Operator, operand: &'static str} = "unknown operator: {operator}{operand}",
    UnknownInfixOperator{left: &'static str, operator: ast::Operator, right: &'static str} = "unknown operator: {left} {operator} {right}",
    TypeMismatch{left: &'static str, operator: ast::Operator, right: &'static str} = "type mismatch: {left} {operator} {right}",
    NotAFunction{type_name: &'static str} = "not a function: {type_name}",
    UnsupportedArgType{fn_name: &'static str, type_name: &'static str} = "argument to `{fn_name}` not supported, got {type_name}",
    IncorrectArity{got: usize, want: usize} = "wrong number of arguments. got={got}, want={want}",
    NotIndexable{type_name: &'static str} = "index operator not supported: {type_name}",
    NotHashable{type_name: &'static str} = "unusable as hash key: {type_name}",
}

impl EvalError {
    pub fn binary_op_error(
        left: &'static str,
        operator: ast::Operator,
        right: &'static str,
    ) -> Self {
        if left == right {
            Self::UnknownInfixOperator {
                left,
                operator,
                right,
            }
        } else {
            Self::TypeMismatch {
                left,
                operator,
                right,
            }
        }
    }
}

pub type Result<T> = std::result::Result<T, EvalError>;

pub type Builtin = fn(Vec<Object>) -> Result<Object>;

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    Function(FunctionObject),
    Builtin(Builtin),
    ReturnValue(Box<Object>),
    Integer(i64),
    Boolean(bool),
    String(String),
    Array(Vec<Object>),
    Hash(HashValue),
    Null,
}

impl Display for Object {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Function(func) => write!(f, "{}", func),
            Self::Builtin(_) => write!(f, "builtin function"),
            Self::ReturnValue(obj) => write!(f, "{}", obj),
            Self::Integer(n) => write!(f, "{}", n),
            Self::Boolean(b) => write!(f, "{}", b),
            Self::String(s) => write!(f, "{}", s),
            Self::Array(a) => {
                let identifier_names: Vec<String> = a.iter().map(Object::to_string).collect();

                write!(f, "[{}]", identifier_names.join(", "))
            }
            Self::Hash(h) => write!(f, "{}", h),
            Self::Null => write!(f, "null"),
        }
    }
}

impl Default for Object {
    fn default() -> Self {
        Self::Null
    }
}

impl From<i64> for Object {
    fn from(n: i64) -> Self {
        Self::Integer(n)
    }
}

impl From<bool> for Object {
    fn from(b: bool) -> Self {
        Self::Boolean(b)
    }
}

impl From<String> for Object {
    fn from(s: String) -> Self {
        Self::String(s)
    }
}

impl From<&str> for Object {
    fn from(s: &str) -> Self {
        s.to_owned().into()
    }
}

impl From<Vec<Object>> for Object {
    fn from(a: Vec<Object>) -> Self {
        Self::Array(a)
    }
}

impl From<HashValue> for Object {
    fn from(h: HashValue) -> Self {
        Self::Hash(h)
    }
}

impl Object {
    pub fn is_return_value(&self) -> bool {
        match self {
            Self::ReturnValue(_) => true,
            _ => false,
        }
    }

    pub fn unwrap_return(self) -> Self {
        match self {
            Self::ReturnValue(o) => *o,
            obj => obj,
        }
    }

    pub fn type_name(&self) -> &'static str {
        match self {
            Self::Function(_) => "FUNCTION",
            Self::Builtin(_) => "BUILTIN",
            Self::ReturnValue(o) => o.type_name(),
            Self::Boolean(_) => "BOOLEAN",
            Self::Integer(_) => "INTEGER",
            Self::String(_) => "STRING",
            Self::Array(_) => "ARRAY",
            Self::Hash(_) => "HASH",
            Self::Null => "NULL",
        }
    }

    pub fn truth_value(self) -> bool {
        match self {
            Self::Boolean(false) => false,
            Self::Null => false,
            _ => true,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionObject {
    pub parameters: Vec<ast::Identifier>,
    pub body: ast::BlockStatement,
    pub env: Environment,
}

impl Display for FunctionObject {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        let identifier_names: Vec<String> = self
            .parameters
            .iter()
            .map(ast::Identifier::to_string)
            .collect();

        write!(f, "fn({}) {}", identifier_names.join(", "), self.body)
    }
}

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

#[derive(Debug, Clone, PartialEq, Default)]
pub struct HashValue {
    pub values: HashMap<HashKey, Object>,
}

impl FromIterator<(HashKey, Object)> for HashValue {
    fn from_iter<I: IntoIterator<Item = (HashKey, Object)>>(iter: I) -> Self {
        Self {
            values: iter.into_iter().collect(),
        }
    }
}

impl Display for HashValue {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let identifier_names: Vec<String> = self
            .values
            .iter()
            .map(|(key, value)| format!("{}: {}", key, value))
            .collect();

        write!(f, "{{{}}}", identifier_names.join(", "))
    }
}
