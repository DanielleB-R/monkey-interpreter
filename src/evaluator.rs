use crate::ast::{self, Node};
use crate::object::Object;

pub fn eval(node: Node) -> Object {
    match node {
        Node::Program(prog) => eval_statements(&prog.statements),
        Node::Statement(s) => match s {
            ast::Statement::Expr(stmt) => eval(stmt.expression.into()),
            _ => Object::Null,
        },
        Node::Expression(e) => match e {
            ast::Expression::IntegerLiteral(l) => Object::Integer(l.value),
            ast::Expression::Boolean(b) => Object::Boolean(b.value),
            ast::Expression::Prefix(prefix) => {
                let right = eval((*prefix.right).into());
                eval_prefix_expression(prefix.operator, right)
            }
            _ => Object::Null,
        },
    }
}

fn eval_statements(statements: &[ast::Statement]) -> Object {
    statements
        .iter()
        .fold(Object::Null, |_, stmt| eval(stmt.clone().into()))
}

fn eval_prefix_expression(operator: String, right: Object) -> Object {
    if operator == "!" {
        eval_bang_operator(right)
    } else if operator == "-" {
        eval_prefix_minus_operator(right)
    } else {
        Object::Null
    }
}

fn eval_bang_operator(right: Object) -> Object {
    Object::Boolean(match right {
        Object::Boolean(true) => (false),
        Object::Boolean(false) => (true),
        Object::Null => (true),
        _ => false,
    })
}

fn eval_prefix_minus_operator(right: Object) -> Object {
    match right {
        Object::Integer(n) => Object::Integer(-n),
        _ => Object::Null,
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    #[test]
    fn test_eval_integer_expression() {
        let cases = vec![("5", 5), ("10", 10), ("-5", -5), ("-10", -10)];

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

    #[test]
    fn test_bang_operator() {
        let cases = vec![
            ("!true", false),
            ("!false", true),
            ("!5", false),
            ("!!true", true),
            ("!!false", false),
            ("!!5", true),
        ];

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
