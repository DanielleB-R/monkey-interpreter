use crate::builtins::Builtin;
use crate::compiler::{Compiler, CompilerState};
use crate::environment::Environment;
use crate::evaluator;
use crate::lexer::Lexer;
use crate::object::Object;
use crate::parser::Parser;
use crate::vm::VM;
use std::io::{self, Write};
use std::rc::Rc;
use strum::IntoEnumIterator;

static PROMPT: &str = ">> ";

pub fn start_interpreted() {
    let mut env = Environment::new();
    loop {
        print!("{}", PROMPT);
        let _ = io::stdout().flush();
        let mut line = String::new();
        if io::stdin().read_line(&mut line).is_err() {
            return;
        }

        match Parser::new(Lexer::new(line)).parse_program() {
            Err(errors) => {
                for err in errors.iter() {
                    println!("\t{}", err);
                }
            }
            Ok(program) => {
                let output = evaluator::eval(program, &mut env);
                match output {
                    Ok(val) => println!("{}", val),
                    Err(err) => println!("ERROR: {}", err),
                }
            }
        }
    }
}

pub fn start() {
    let mut compiler_state = CompilerState::default();
    for builtin in Builtin::iter() {
        compiler_state
            .symbol_table
            .define_builtin(builtin as isize, &builtin.to_string());
    }
    let mut vm_state: Vec<Rc<Object>> = vec![Rc::new(Object::Null); 65536];
    loop {
        print!("{}", PROMPT);
        let _ = io::stdout().flush();
        let mut line = String::new();
        if io::stdin().read_line(&mut line).is_err() {
            return;
        }

        match Parser::new(Lexer::new(line)).parse_program() {
            Err(errors) => {
                for err in errors.iter() {
                    println!("\t{}", err);
                }
            }
            Ok(program) => {
                let mut comp: Compiler = compiler_state.clone().into();
                if comp.compile(program).is_err() {
                    println!("Compilation error!");
                    continue;
                }

                compiler_state = comp.save_state();

                let mut machine = VM::with_state(comp.bytecode(), vm_state.clone());
                if machine.run().is_err() {
                    println!("Execution error!");
                }

                println!("{}", machine.last_popped_stack_element());
                vm_state = machine.into_state();
            }
        }
    }
}
