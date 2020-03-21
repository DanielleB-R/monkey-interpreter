use crate::ast;
use crate::environment::Environment;
use custom_error::custom_error;
use std::fmt::{self, Display, Formatter};

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
            Self::Null => write!(f, "null"),
        }
    }
}

impl Default for Object {
    fn default() -> Self {
        Self::Null
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
            Self::Null => "NULL",
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
