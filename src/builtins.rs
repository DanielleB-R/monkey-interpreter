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
        Object::String(s) => Ok(Object::Integer(s.len() as i64)),
        obj => Err(EvalError::UnsupportedArgType {
            fn_name: "len",
            type_name: obj.type_name(),
        }),
    }
}

lazy_static! {
    pub static ref BUILTINS: HashMap<String, Object> =
        vec![("len".to_owned(), Object::Builtin(len))]
            .into_iter()
            .collect();
}
