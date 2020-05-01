use crate::ast;
use crate::builtins::Builtin;
use crate::code::Instructions;
use crate::environment::Environment;
use std::collections::HashMap;
use std::fmt::{self, Display, Formatter};
use std::iter::FromIterator;
use std::rc::Rc;

mod eval_error;
pub use eval_error::EvalError;

mod hash;
pub use hash::HashKey;

pub type Result<T> = std::result::Result<T, EvalError>;

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    Function(FunctionObject),
    CompiledFunction(CompiledFunction),
    Closure(Closure),
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
            Self::CompiledFunction(func) => write!(f, "{}", func),
            Self::Closure(func) => write!(f, "{}", func),
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

impl From<CompiledFunction> for Object {
    fn from(f: CompiledFunction) -> Self {
        Self::CompiledFunction(f)
    }
}

impl From<Closure> for Object {
    fn from(f: Closure) -> Self {
        Self::Closure(f)
    }
}

impl From<Instructions> for Object {
    fn from(i: Instructions) -> Self {
        CompiledFunction {
            instructions: i,
            num_locals: 0,
            num_parameters: 0,
        }
        .into()
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
            Self::CompiledFunction(_) => "COMPILED_FUNCTION",
            Self::Closure(_) => "CLOSURE",
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

    pub fn truth_value(&self) -> bool {
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
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let identifier_names: Vec<String> = self
            .parameters
            .iter()
            .map(ast::Identifier::to_string)
            .collect();

        write!(f, "fn({}) {}", identifier_names.join(", "), self.body)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompiledFunction {
    pub instructions: Instructions,
    pub num_locals: isize,
    pub num_parameters: usize,
}

impl Display for CompiledFunction {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "CompiledFunction[{}]", self.instructions)
    }
}

impl CompiledFunction {
    pub fn new(instructions: Instructions, num_locals: isize, num_parameters: usize) -> Self {
        Self {
            instructions,
            num_locals,
            num_parameters,
        }
    }
}
#[derive(Debug, Clone, PartialEq)]
pub struct Closure {
    pub func: CompiledFunction,
    pub free: Vec<Rc<Object>>,
}

impl From<CompiledFunction> for Closure {
    fn from(func: CompiledFunction) -> Self {
        Self {
            func,
            free: Default::default(),
        }
    }
}

impl Display for Closure {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "Closure[{}]", self.func)
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
