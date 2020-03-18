use crate::ast;
use crate::environment::Environment;
use std::fmt::{self, Display, Formatter};

use custom_error::custom_error;

custom_error! {
    #[derive(Clone, PartialEq)]
    pub EvalError

    IdentifierNotFound{id: String} = "identifier not found: {id}",
    UnknownPrefixOperator{operator: ast::Operator, operand: &'static str} = "unknown operator: {operator}{operand}",
    UnknownInfixOperator{left: &'static str, operator: ast::Operator, right: &'static str} = "unknown operator: {left} {operator} {right}",
    TypeMismatch{left: &'static str, operator: ast::Operator, right: &'static str} = "type mismatch: {left} {operator} {right}",
    NotAFunction{type_name: &'static str} = "not a function: {type_name}",
}

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    Function(FunctionObject),
    ReturnValue(Box<Object>),
    // TODO: Make this a Rust error and use Results
    Error(EvalError),
    Integer(i64),
    Boolean(bool),
    Null,
}

impl Display for Object {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Function(func) => write!(f, "{}", func),
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

    pub fn type_name(&self) -> &'static str {
        match self {
            Self::Function(_) => "FUNCTION",
            Self::ReturnValue(o) => o.type_name(),
            Self::Error(_) => "ERROR",
            Self::Boolean(_) => "BOOLEAN",
            Self::Integer(_) => "INTEGER",
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
