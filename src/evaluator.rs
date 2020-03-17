use crate::ast::{self, Node};
use crate::object::Object;

pub fn eval(node: Node) -> Object {
    match node {
        Node::Program(prog) => eval_program(prog),
        Node::Statement(s) => match s {
            ast::Statement::Expr(stmt) => eval(stmt.expression.into()),
            ast::Statement::Block(stmt) => eval_block_statement(stmt),
            ast::Statement::Return(stmt) => {
                let val = eval(stmt.return_value.into());
                if val.is_error() {
                    return val;
                }
                Object::ReturnValue(Box::new(val))
            }
            _ => Object::Null,
        },
        Node::Expression(e) => match e {
            ast::Expression::IntegerLiteral(l) => Object::Integer(l.value),
            ast::Expression::Boolean(b) => Object::Boolean(b.value),
            ast::Expression::Prefix(prefix) => {
                let right = eval((*prefix.right).into());
                if right.is_error() {
                    return right;
                }
                eval_prefix_expression(prefix.operator, right)
            }
            ast::Expression::Infix(infix) => {
                let left = eval((*infix.left).into());
                if left.is_error() {
                    return left;
                }
                let right = eval((*infix.right).into());
                if right.is_error() {
                    return right;
                }
                eval_infix_expression(infix.operator, left, right)
            }
            ast::Expression::If(if_expression) => eval_if_expression(if_expression),
            _ => Object::Null,
        },
    }
}

fn eval_program(program: ast::Program) -> Object {
    let mut result = Object::Null;

    for stmt in program.statements.into_iter() {
        result = eval(stmt.into());

        if let Object::ReturnValue(obj) = result {
            return *obj;
        }

        if result.is_error() {
            return result;
        }
    }

    result
}

fn eval_block_statement(block: ast::BlockStatement) -> Object {
    let mut result = Object::Null;

    for stmt in block.statements.into_iter() {
        result = eval(stmt.into());

        if result.is_return_value() || result.is_error() {
            return result;
        }
    }

    result
}

fn eval_prefix_expression(operator: ast::Operator, right: Object) -> Object {
    match operator {
        ast::Operator::Bang => eval_bang_operator(right),
        ast::Operator::Minus => eval_prefix_minus_operator(right),
        _ => Object::Error(format!(
            "unknown operator: {}{}",
            operator,
            right.type_name()
        )),
    }
}

fn eval_infix_expression(operator: ast::Operator, left: Object, right: Object) -> Object {
    match operator {
        ast::Operator::Eq => Object::Boolean(left == right),
        ast::Operator::NotEq => Object::Boolean(left != right),
        op => match (left, right) {
            (Object::Integer(x), Object::Integer(y)) => eval_integer_infix_expression(op, x, y),
            (Object::Boolean(_), Object::Boolean(_)) => {
                Object::Error(format!("unknown operator: BOOLEAN {} BOOLEAN", op))
            }
            (a, b) => Object::Error(format!(
                "type mismatch: {} {} {}",
                a.type_name(),
                op,
                b.type_name()
            )),
        },
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
        a => Object::Error(format!("unknown operator: -{}", a.type_name())),
    }
}

fn eval_integer_infix_expression(operator: ast::Operator, left: i64, right: i64) -> Object {
    match operator {
        ast::Operator::Plus => Object::Integer(left + right),
        ast::Operator::Minus => Object::Integer(left - right),
        ast::Operator::Asterisk => Object::Integer(left * right),
        ast::Operator::Slash => Object::Integer(left / right),
        ast::Operator::LT => Object::Boolean(left < right),
        ast::Operator::GT => Object::Boolean(left > right),
        ast::Operator::Eq => Object::Boolean(left == right),
        ast::Operator::NotEq => Object::Boolean(left != right),
        op => Object::Error(format!("unknown operator: INTEGER {} INTEGER", op)),
    }
}

fn eval_if_expression(if_expression: ast::IfExpression) -> Object {
    let condition = eval((*if_expression.condition).into());
    if condition.is_error() {
        return condition;
    }

    if is_truthy(condition) {
        eval(ast::Statement::Block(if_expression.consequence).into())
    } else if let Some(alt) = if_expression.alternative {
        eval(ast::Statement::Block(alt).into())
    } else {
        Object::Null
    }
}

fn is_truthy(obj: Object) -> bool {
    obj != Object::Null && obj != Object::Boolean(false)
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    #[test]
    fn test_eval_integer_expression() {
        let cases = vec![
            ("5", 5),
            ("10", 10),
            ("-5", -5),
            ("-10", -10),
            ("5 + 5 + 5 + 5 - 10", 10),
            ("2 * 2 * 2 * 2 * 2", 32),
            ("-50 + 100 + -50", 0),
            ("5 * 2 + 10", 20),
            ("5 + 2 * 10", 25),
            ("20 + 2 * -10", 0),
            ("50 / 2 * 2 + 10", 60),
            ("2 * (5 + 10)", 30),
            ("3 * 3 * 3 + 10", 37),
            ("3 * (3 * 3) + 10", 37),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
        ];

        for (input, output) in cases.into_iter() {
            let evaluated = test_eval(input);
            test_integer_object(&evaluated, output);
        }
    }

    #[test]
    fn test_eval_boolean_expression() {
        let cases = vec![
            ("true", true),
            ("false", false),
            ("1 < 2", true),
            ("1 > 2", false),
            ("1 < 1", false),
            ("1 > 1", false),
            ("1 == 1", true),
            ("1 != 1", false),
            ("1 == 2", false),
            ("1 != 2", true),
            ("true == true", true),
            ("false == false", true),
            ("true == false", false),
            ("true != false", true),
            ("false != true", true),
            ("(1 < 2) == true", true),
            ("(1 < 2) == false", false),
            ("(1 > 2) == true", false),
            ("(1 > 2) == false", true),
        ];

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

    #[test]
    fn test_if_else_expressions() {
        let cases = vec![
            ("if (true) { 10 }", Object::Integer(10)),
            ("if (false) { 10 }", Object::Null),
            ("if (1) { 10 }", Object::Integer(10)),
            ("if (1 < 2) { 10 }", Object::Integer(10)),
            ("if (1 > 2) { 10 }", Object::Null),
            ("if (1 < 2) { 10 } else { 20 }", Object::Integer(10)),
            ("if (1 > 2) { 10 } else { 20 }", Object::Integer(20)),
        ];

        for (input, output) in cases.into_iter() {
            let evaluated = test_eval(input);
            assert_eq!(evaluated, output);
        }
    }

    #[test]
    fn test_return_statements() {
        let cases = vec![
            ("return 10;", 10),
            ("return 10; 9;", 10),
            ("return 2 * 5; 9;", 10),
            ("9; return 2 * 5; 9;", 10),
            (
                "if (10 > 1) {
  if (10 > 1) {
    return 10;
  }

  return 1;
}",
                10,
            ),
        ];

        for (input, output) in cases.into_iter() {
            let evaluated = test_eval(input);
            test_integer_object(&evaluated, output);
        }
    }

    #[test]
    fn test_error_handling() {
        let cases = vec![
            ("5 + true;", "type mismatch: INTEGER + BOOLEAN"),
            ("5 + true; 5;", "type mismatch: INTEGER + BOOLEAN"),
            ("-true", "unknown operator: -BOOLEAN"),
            ("true + false;", "unknown operator: BOOLEAN + BOOLEAN"),
            ("5; true + false; 5", "unknown operator: BOOLEAN + BOOLEAN"),
            (
                "if (10 > 1) { true + false; }",
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
            (
                "
if (10 > 1) {
  if (10 > 1) {
    return true + false;
  }

  return 1;
}",
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
        ];

        for (input, err) in cases.into_iter() {
            let evaluated = test_eval(input);
            assert_eq!(evaluated, Object::Error(err.to_owned()));
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
