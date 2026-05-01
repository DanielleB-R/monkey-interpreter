use crate::object::*;
use strum_macros::{Display, EnumIter, EnumString};

fn len(args: Vec<Object>) -> Result<Object> {
    if args.len() != 1 {
        return Err(EvalError::IncorrectArity {
            got: args.len(),
            want: 1,
        });
    }

    match args.into_iter().next().unwrap() {
        Object::Array(a) => Ok((a.len() as i64).into()),
        Object::String(s) => Ok((s.len() as i64).into()),
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
            if a.is_empty() {
                Ok(Object::Null)
            } else {
                Ok(Object::Array(a.into_iter().skip(1).collect()))
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
            let mut result = a;
            result.push(args_iter.next().unwrap());
            Ok(Object::Array(result))
        }
        obj => Err(EvalError::UnsupportedArgType {
            fn_name: "push",
            type_name: obj.type_name(),
        }),
    }
}

fn puts(args: Vec<Object>) -> Result<Object> {
    for arg in args.into_iter() {
        println!("{}", arg);
    }

    Ok(Object::Null)
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Display, EnumString, EnumIter)]
#[strum(serialize_all = "snake_case")]
pub enum Builtin {
    Len,
    Puts,
    First,
    Last,
    Rest,
    Push,
}

impl Builtin {
    pub fn func(self) -> fn(Vec<Object>) -> Result<Object> {
        match self {
            Self::Len => len,
            Self::Puts => puts,
            Self::First => first,
            Self::Last => last,
            Self::Rest => rest,
            Self::Push => push,
        }
    }
}
