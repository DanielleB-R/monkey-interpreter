use crate::lexer::Lexer;
use crate::parser::Parser;
use std::io::{self, Write};

static PROMPT: &str = ">> ";

pub fn start() {
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
            Ok(program) => println!("{}", program),
        }
    }
}
