use crate::ast::{self, Node};
use crate::lexer::Lexer;
use crate::object::Object;
use crate::parser::Parser;

pub fn eval(node: Node) -> Object {
    match node {
        Node::Program(prog) => eval_statements(&prog.statements),
        Node::Statement(s) => match s {
            ast::Statement::Expr(stmt) => eval(stmt.expression.into()),
            _ => panic!(),
        },
        Node::Expression(e) => match e {
            ast::Expression::IntegerLiteral(l) => Object::Integer(l.value),
            ast::Expression::Boolean(b) => Object::Boolean(b.value),
            _ => panic!(),
        },
    }
}

fn eval_statements(statements: &[ast::Statement]) -> Object {
    statements
        .iter()
        .fold(Object::Null, |_, stmt| eval(stmt.clone().into()))
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_eval_integer_expression() {
        let cases = vec![("5", 5), ("10", 10)];

        for (input, output) in cases.into_iter() {
            let evaluated = test_eval(input);
            test_integer_object(&evaluated, output);
        }
    }

    #[test]
    fn test_eval_boolean_expression() {
        let cases = vec![("true", true), ("false", false)];

        for (input, output) in cases.into_iter() {
            let evaluated = test_eval(input);
            test_boolean_object(&evaluated, output);
        }
    }

    fn test_eval(input: &str) -> Object {
        eval(
            Parser::new(Lexer::new(input.to_owned()))
                .parse_program()
                .expect("Parse errors found")
                .into(),
        )
    }

    fn test_integer_object(obj: &Object, expected: i64) {
        match obj {
            Object::Integer(n) => assert_eq!(*n, expected),
            _ => panic!(),
        }
    }

    fn test_boolean_object(obj: &Object, expected: bool) {
        match obj {
            Object::Boolean(b) => assert_eq!(*b, expected),
            _ => panic!(),
        }
    }
}
