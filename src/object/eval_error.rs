use crate::ast;
use custom_error::custom_error;

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
