use crate::ast::{self, Node};
use crate::builtins;
use crate::environment::Environment;
use crate::object::{EvalError, FunctionObject, HashKey, HashValue, Object, Result};
use lazy_static::lazy_static;
use std::collections::HashMap;
use std::convert::TryInto;

lazy_static! {
    pub static ref BUILTINS: HashMap<String, Object> = builtins::BUILTINS.iter().cloned().collect();
}

pub fn eval(node: Node, env: &mut Environment) -> Result<Object> {
    match node {
        Node::Program(prog) => eval_program(prog, env),
        Node::Statement(s) => match s {
            ast::Statement::Expr(stmt) => eval(stmt.expression.into(), env),
            ast::Statement::Block(stmt) => eval_block_statement(stmt, env),
            ast::Statement::Return(stmt) => Ok(Object::ReturnValue(Box::new(eval(
                stmt.return_value.into(),
                env,
            )?))),
            ast::Statement::Let(stmt) => {
                let val = eval(stmt.value.into(), env)?;
                env.set(&stmt.name.value, val);

                Ok(Object::Null)
            }
        },
        Node::Expression(e) => match e {
            ast::Expression::IntegerLiteral(l) => Ok(l.value.into()),
            ast::Expression::Boolean(b) => Ok(b.value.into()),
            ast::Expression::Prefix(prefix) => {
                let right = eval((*prefix.right).into(), env)?;
                eval_prefix_expression(prefix.operator, right)
            }
            ast::Expression::Infix(infix) => {
                let left = eval((*infix.left).into(), env)?;
                let right = eval((*infix.right).into(), env)?;
                eval_infix_expression(infix.operator, left, right)
            }
            ast::Expression::If(if_expression) => eval_if_expression(if_expression, env),
            ast::Expression::Identifier(identifier) => env
                .get(&identifier.value)
                .or_else(|| BUILTINS.get(&identifier.value).cloned())
                .ok_or_else(|| EvalError::IdentifierNotFound {
                    id: identifier.value.clone(),
                }),
            ast::Expression::Function(fn_literal) => Ok(Object::Function(FunctionObject {
                parameters: fn_literal.parameters,
                body: fn_literal.body,
                env: env.clone(),
            })),
            ast::Expression::Call(call) => {
                let function = eval((*call.function).into(), env)?;
                let args = eval_expressions(call.arguments, env)?;
                apply_function(function, args)
            }
            ast::Expression::String(s) => Ok(Object::String(s.value)),
            ast::Expression::Array(a) => eval_expressions(a.elements, env).map(Object::Array),
            ast::Expression::Hash(h) => eval_hash_literal(h.pairs, env),
            ast::Expression::Index(i) => {
                let left = eval((*i.left).into(), env)?;
                let index = eval((*i.index).into(), env)?;
                eval_index_expression(left, index)
            }
            _ => Ok(Object::Null),
        },
    }
}

fn eval_program(program: ast::Program, env: &mut Environment) -> Result<Object> {
    let mut result = Object::default();

    for stmt in program.statements.into_iter() {
        result = eval(stmt.into(), env)?;

        if let Object::ReturnValue(obj) = result {
            return Ok(*obj);
        }
    }

    Ok(result)
}

fn eval_block_statement(block: ast::BlockStatement, env: &mut Environment) -> Result<Object> {
    let mut result = Object::default();

    for stmt in block.statements.into_iter() {
        result = eval(stmt.into(), env)?;

        if result.is_return_value() {
            return Ok(result);
        }
    }

    Ok(result)
}

fn eval_expressions(exprs: Vec<ast::Expression>, env: &mut Environment) -> Result<Vec<Object>> {
    let mut result = vec![];

    for expr in exprs.into_iter() {
        result.push(eval(expr.into(), env)?)
    }
    Ok(result)
}

fn eval_prefix_expression(operator: ast::Operator, right: Object) -> Result<Object> {
    match operator {
        ast::Operator::Bang => Ok(eval_bang_operator(right)),
        ast::Operator::Minus => eval_prefix_minus_operator(right),
        _ => Err(EvalError::UnknownPrefixOperator {
            operator,
            operand: right.type_name(),
        }),
    }
}

fn eval_infix_expression(operator: ast::Operator, left: Object, right: Object) -> Result<Object> {
    match operator {
        ast::Operator::Eq => Ok((left == right).into()),
        ast::Operator::NotEq => Ok((left != right).into()),
        op => match (left, right) {
            (Object::Integer(x), Object::Integer(y)) => eval_integer_infix_expression(op, x, y),
            (Object::String(a), Object::String(b)) => eval_string_infix_expression(op, a, b),
            (a, b) => Err(EvalError::binary_op_error(a.type_name(), op, b.type_name())),
        },
    }
}

fn eval_bang_operator(right: Object) -> Object {
    (!right.truth_value()).into()
}

fn eval_prefix_minus_operator(right: Object) -> Result<Object> {
    match right {
        Object::Integer(n) => Ok((-n).into()),
        a => Err(EvalError::UnknownPrefixOperator {
            operator: ast::Operator::Minus,
            operand: a.type_name(),
        }),
    }
}

fn eval_integer_infix_expression(operator: ast::Operator, left: i64, right: i64) -> Result<Object> {
    match operator {
        ast::Operator::Plus => Ok((left + right).into()),
        ast::Operator::Minus => Ok((left - right).into()),
        ast::Operator::Asterisk => Ok((left * right).into()),
        ast::Operator::Slash => Ok((left / right).into()),
        ast::Operator::LT => Ok((left < right).into()),
        ast::Operator::GT => Ok((left > right).into()),
        ast::Operator::Eq => Ok((left == right).into()),
        ast::Operator::NotEq => Ok((left != right).into()),
        op => Err(EvalError::UnknownInfixOperator {
            left: "INTEGER",
            operator: op,
            right: "INTEGER",
        }),
    }
}

fn eval_string_infix_expression(
    operator: ast::Operator,
    left: String,
    right: String,
) -> Result<Object> {
    match operator {
        ast::Operator::Plus => Ok(Object::String(left + &right)),
        op => Err(EvalError::UnknownInfixOperator {
            left: "STRING",
            operator: op,
            right: "STRING",
        }),
    }
}

fn eval_if_expression(if_expression: ast::IfExpression, env: &mut Environment) -> Result<Object> {
    let condition = eval((*if_expression.condition).into(), env)?;

    if condition.truth_value() {
        eval(ast::Statement::Block(if_expression.consequence).into(), env)
    } else if let Some(alt) = if_expression.alternative {
        eval(ast::Statement::Block(alt).into(), env)
    } else {
        Ok(Object::Null)
    }
}

fn apply_function(func: Object, args: Vec<Object>) -> Result<Object> {
    match func {
        Object::Function(f) => {
            let mut env = extend_function_env(&f, args);

            eval(ast::Statement::Block(f.body).into(), &mut env).map(Object::unwrap_return)
        }
        Object::Builtin(f) => f(args),
        obj => Err(EvalError::NotAFunction {
            type_name: obj.type_name(),
        }),
    }
}

fn extend_function_env(func: &FunctionObject, args: Vec<Object>) -> Environment {
    let mut env = Environment::with_enclosed(&func.env);

    for (param, arg) in func.parameters.iter().zip(args.into_iter()) {
        env.set(&param.value, arg);
    }

    env
}

fn eval_index_expression(array: Object, index: Object) -> Result<Object> {
    match (array, index) {
        (Object::Array(arr), Object::Integer(n)) => eval_array_index_expression(arr, n),
        (Object::Hash(h), ind) => eval_hash_index_expression(h, ind),
        (a, _) => Err(EvalError::NotIndexable {
            type_name: a.type_name(),
        }),
    }
}

fn eval_array_index_expression(array: Vec<Object>, index: i64) -> Result<Object> {
    if index >= 0 && index < array.len() as i64 {
        Ok(array.into_iter().nth(index as usize).unwrap())
    } else {
        Ok(Object::Null)
    }
}

fn eval_hash_index_expression(hash: HashValue, index: Object) -> Result<Object> {
    Ok(hash
        .values
        .get(&index.try_into()?)
        .cloned()
        .unwrap_or_default())
}

fn eval_hash_literal(
    hash: Vec<(ast::Expression, ast::Expression)>,
    env: &mut Environment,
) -> Result<Object> {
    let mut map = HashMap::new();

    for (key_expr, val_expr) in hash.into_iter() {
        let key = eval(key_expr.into(), env)?;
        let value = eval(val_expr.into(), env)?;

        map.insert(key.try_into()?, value);
    }

    Ok(Object::Hash(HashValue { values: map }))
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
            assert_eq!(test_eval(input).unwrap(), output.into());
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
            assert_eq!(test_eval(input).unwrap(), output.into());
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
            assert_eq!(test_eval(input).unwrap(), output.into());
        }
    }

    #[test]
    fn test_if_else_expressions() {
        let cases = vec![
            ("if (true) { 10 }", 10.into()),
            ("if (false) { 10 }", Object::Null),
            ("if (1) { 10 }", 10.into()),
            ("if (1 < 2) { 10 }", 10.into()),
            ("if (1 > 2) { 10 }", Object::Null),
            ("if (1 < 2) { 10 } else { 20 }", 10.into()),
            ("if (1 > 2) { 10 } else { 20 }", 20.into()),
        ];

        for (input, output) in cases.into_iter() {
            assert_eq!(test_eval(input).unwrap(), output);
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
            assert_eq!(test_eval(input).unwrap(), output.into());
        }
    }

    #[test]
    fn test_error_handling() {
        let cases = vec![
            (
                "5 + true;",
                EvalError::TypeMismatch {
                    left: "INTEGER",
                    operator: ast::Operator::Plus,
                    right: "BOOLEAN",
                },
            ),
            (
                "5 + true; 5;",
                EvalError::TypeMismatch {
                    left: "INTEGER",
                    operator: ast::Operator::Plus,
                    right: "BOOLEAN",
                },
            ),
            (
                "-true",
                EvalError::UnknownPrefixOperator {
                    operator: ast::Operator::Minus,
                    operand: "BOOLEAN",
                },
            ),
            (
                "true + false;",
                EvalError::UnknownInfixOperator {
                    left: "BOOLEAN",
                    operator: ast::Operator::Plus,
                    right: "BOOLEAN",
                },
            ),
            (
                "5; true + false; 5",
                EvalError::UnknownInfixOperator {
                    left: "BOOLEAN",
                    operator: ast::Operator::Plus,
                    right: "BOOLEAN",
                },
            ),
            (
                "if (10 > 1) { true + false; }",
                EvalError::UnknownInfixOperator {
                    left: "BOOLEAN",
                    operator: ast::Operator::Plus,
                    right: "BOOLEAN",
                },
            ),
            (
                "foobar",
                EvalError::IdentifierNotFound {
                    id: "foobar".to_owned(),
                },
            ),
            (
                "
if (10 > 1) {
  if (10 > 1) {
    return true + false;
  }

  return 1;
}",
                EvalError::UnknownInfixOperator {
                    left: "BOOLEAN",
                    operator: ast::Operator::Plus,
                    right: "BOOLEAN",
                },
            ),
            (
                "\"Hello\" - \"World\"",
                EvalError::UnknownInfixOperator {
                    left: "STRING",
                    operator: ast::Operator::Minus,
                    right: "STRING",
                },
            ),
            (
                "{\"name\": \"Monkey\"}[fn(x) { x }];",
                EvalError::NotHashable {
                    type_name: "FUNCTION",
                },
            ),
        ];

        for (input, err) in cases.into_iter() {
            assert_eq!(test_eval(input).unwrap_err(), err);
        }
    }

    #[test]
    fn test_let_statements() {
        let cases = vec![
            ("let a = 5; a;", 5),
            ("let a = 5 * 5; a;", 25),
            ("let a = 5; let b = a; b;", 5),
            ("let a = 5; let b = a; let c = a + b + 5; c;", 15),
        ];

        for (input, val) in cases.into_iter() {
            assert_eq!(test_eval(input).unwrap(), val.into());
        }
    }

    #[test]
    fn test_function_object() {
        let input = "fn(x) { x + 2; }";

        let fn_obj = match test_eval(input).unwrap() {
            Object::Function(f) => f,
            _ => panic!("not a function"),
        };

        assert_eq!(fn_obj.parameters.len(), 1);
        assert_eq!(fn_obj.parameters[0].to_string(), "x");
        assert_eq!(fn_obj.body.to_string(), "(x + 2)");
    }

    #[test]
    fn test_function_application() {
        let cases = vec![
            ("let identity = fn(x) { x; }; identity(5);", 5),
            ("let identity = fn(x) { return x; }; identity(5);", 5),
            ("let double = fn(x) { x * 2; }; double(5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
            ("fn(x) { x; }5;", 5),
        ];

        for (input, output) in cases.into_iter() {
            assert_eq!(test_eval(input).unwrap(), output.into());
        }
    }

    #[test]
    fn test_closures() {
        let input = "
let newAdder = fn(x) {
  fn(y) { x + y; };
};

let addTwo = newAdder(2);
addTwo(2);
";

        assert_eq!(test_eval(input).unwrap(), 4.into());
    }

    #[test]
    fn test_string_literal() {
        let input = "\"Hello World!\"";
        match test_eval(input).unwrap() {
            Object::String(s) => assert_eq!(s, "Hello World!"),
            _ => panic!(),
        }
    }

    #[test]
    fn test_string_concatenation() {
        let input = "\"Hello\" +\" World!\"";
        match test_eval(input).unwrap() {
            Object::String(s) => assert_eq!(s, "Hello World!"),
            _ => panic!(),
        }
    }

    #[test]
    fn test_builtin_functions() {
        let cases = vec![
            ("len(\"\")", Ok(0.into())),
            ("len(\"four\")", Ok(Object::Integer(4))),
            ("len(\"hello world\")", Ok(Object::Integer(11))),
            (
                "len(1)",
                Err(EvalError::UnsupportedArgType {
                    fn_name: "len",
                    type_name: "INTEGER",
                }),
            ),
            (
                "len(\"one\", \"two\")",
                Err(EvalError::IncorrectArity { got: 2, want: 1 }),
            ),
            ("len([1, 2, 3])", Ok(Object::Integer(3))),
            ("len([])", Ok(Object::Integer(0))),
            ("first([1, 2, 3])", Ok(Object::Integer(1))),
            ("first([])", Ok(Object::Null)),
            (
                "first(1)",
                Err(EvalError::UnsupportedArgType {
                    fn_name: "first",
                    type_name: "INTEGER",
                }),
            ),
            ("last([1, 2, 3])", Ok(Object::Integer(3))),
            ("last([])", Ok(Object::Null)),
            (
                "last(1)",
                Err(EvalError::UnsupportedArgType {
                    fn_name: "last",
                    type_name: "INTEGER",
                }),
            ),
            (
                "rest([1, 2, 3])",
                Ok(Object::Array(vec![Object::Integer(2), Object::Integer(3)])),
            ),
            ("rest([])", Ok(Object::Null)),
            ("push([], 1)", Ok(Object::Array(vec![Object::Integer(1)]))),
            (
                "push(1, 1)",
                Err(EvalError::UnsupportedArgType {
                    fn_name: "push",
                    type_name: "INTEGER",
                }),
            ),
        ];

        for (input, output) in cases.into_iter() {
            assert_eq!(test_eval(input), output);
        }
    }

    #[test]
    fn test_array_literals() {
        let input = "[1, 2 * 2, 3 + 3]";

        let arr_obj = match test_eval(input).unwrap() {
            Object::Array(a) => a,
            _ => panic!("not an array"),
        };

        assert_eq!(arr_obj.len(), 3);
        assert_eq!(arr_obj[0], Object::Integer(1));
        assert_eq!(arr_obj[1], Object::Integer(4));
        assert_eq!(arr_obj[2], Object::Integer(6));
    }

    #[test]
    fn test_array_index_expressions() {
        let cases = vec![
            ("[1, 2, 3][0]", Object::Integer(1)),
            ("[1, 2, 3][1]", Object::Integer(2)),
            ("[1, 2, 3][2]", Object::Integer(3)),
            ("let i = 0; [1][i];", Object::Integer(1)),
            ("[1, 2, 3][1 + 1];", Object::Integer(3)),
            ("let myArray = [1, 2, 3]; myArray[2];", Object::Integer(3)),
            (
                "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
                Object::Integer(6),
            ),
            (
                "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i];",
                Object::Integer(2),
            ),
            ("[1, 2, 3][3]", Object::Null),
            ("[1, 2, 3][-1]", Object::Null),
        ];

        for (input, output) in cases.into_iter() {
            assert_eq!(test_eval(input).unwrap(), output);
        }
    }

    #[test]
    fn test_hash_literals() {
        let input = "let two = \"two\";
{
  \"one\": 10 - 9,
  two: 1 + 1,
  \"thr\" + \"ee\": 6 / 2,
  4: 4,
  true: 5,
  false: 6
}";

        assert_eq!(
            test_eval(input).unwrap(),
            Object::Hash(HashValue {
                values: vec![
                    (HashKey::String("one".to_owned()), Object::Integer(1)),
                    (HashKey::String("two".to_owned()), Object::Integer(2)),
                    (HashKey::String("three".to_owned()), Object::Integer(3)),
                    (HashKey::Integer(4), Object::Integer(4)),
                    (HashKey::Boolean(true), Object::Integer(5)),
                    (HashKey::Boolean(false), Object::Integer(6)),
                ]
                .into_iter()
                .collect()
            })
        );
    }

    #[test]
    fn test_hash_index_expressions() {
        let cases = vec![
            ("{\"foo\": 5}[\"foo\"]", Object::Integer(5)),
            ("{\"foo\": 5}[\"bar\"]", Object::Null),
            ("let key = \"foo\"; {\"foo\": 5}[key]", Object::Integer(5)),
            ("{}[\"foo\"]", Object::Null),
            ("{5: 5}[5]", Object::Integer(5)),
        ];

        for (input, output) in cases.into_iter() {
            assert_eq!(test_eval(input).unwrap(), output);
        }
    }

    fn test_eval(input: &str) -> Result<Object> {
        let mut env = Environment::new();
        eval(
            Parser::new(Lexer::new(input.to_owned()))
                .parse_program()
                .expect("Parse errors found")
                .into(),
            &mut env,
        )
    }
}
