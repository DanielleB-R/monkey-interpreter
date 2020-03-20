use crate::object::*;
use lazy_static::lazy_static;
use std::collections::HashMap;

fn len(args: Vec<Object>) -> Result<Object> {
    if args.len() != 1 {
        return Err(EvalError::IncorrectArity {
            got: args.len(),
            want: 1,
        });
    }

    match args.into_iter().next().unwrap() {
        Object::Array(a) => Ok(Object::Integer(a.len() as i64)),
        Object::String(s) => Ok(Object::Integer(s.len() as i64)),
        obj => Err(EvalError::UnsupportedArgType {
            fn_name: "len",
            type_name: obj.type_name(),
        }),
    }
}

fn first(args: Vec<Object>) -> Result<Object> {
    if args.len() != 1 {
        return Err(EvalError::IncorrectArity {
            got: args.len(),
            want: 1,
        });
    }

    match args.into_iter().next().unwrap() {
        Object::Array(a) => Ok(a.into_iter().next().unwrap_or_default()),
        obj => Err(EvalError::UnsupportedArgType {
            fn_name: "first",
            type_name: obj.type_name(),
        }),
    }
}

fn last(args: Vec<Object>) -> Result<Object> {
    if args.len() != 1 {
        return Err(EvalError::IncorrectArity {
            got: args.len(),
            want: 1,
        });
    }

    match args.into_iter().next().unwrap() {
        Object::Array(a) => Ok(a.into_iter().last().unwrap_or_default()),
        obj => Err(EvalError::UnsupportedArgType {
            fn_name: "last",
            type_name: obj.type_name(),
        }),
    }
}

fn rest(args: Vec<Object>) -> Result<Object> {
    if args.len() != 1 {
        return Err(EvalError::IncorrectArity {
            got: args.len(),
            want: 1,
        });
    }

    match args.into_iter().next().unwrap() {
        Object::Array(a) => {
            if a.len() > 0 {
                Ok(Object::Array(a.into_iter().skip(1).collect()))
            } else {
                Ok(Object::Null)
            }
        }
        obj => Err(EvalError::UnsupportedArgType {
            fn_name: "rest",
            type_name: obj.type_name(),
        }),
    }
}

fn push(args: Vec<Object>) -> Result<Object> {
    if args.len() != 2 {
        return Err(EvalError::IncorrectArity {
            got: args.len(),
            want: 1,
        });
    }

    let mut args_iter = args.into_iter();
    match args_iter.next().unwrap() {
        Object::Array(a) => {
            let mut result = a.clone();
            result.push(args_iter.next().unwrap());
            Ok(Object::Array(result))
        }
        obj => Err(EvalError::UnsupportedArgType {
            fn_name: "push",
            type_name: obj.type_name(),
        }),
    }
}

lazy_static! {
    pub static ref BUILTINS: HashMap<String, Object> = vec![
        ("len".to_owned(), Object::Builtin(len)),
        ("first".to_owned(), Object::Builtin(first)),
        ("last".to_owned(), Object::Builtin(last)),
        ("rest".to_owned(), Object::Builtin(rest)),
        ("push".to_owned(), Object::Builtin(push))
    ]
    .into_iter()
    .collect();
}
