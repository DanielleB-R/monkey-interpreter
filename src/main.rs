mod ast;
mod builtins;
mod code;
mod compiler;
mod environment;
mod evaluator;
mod lexer;
mod object;
mod parser;
mod repl;
mod token;
mod vm;

use std::env;

fn main() {
    println!("Hello! This is the Monkey programming language!");
    println!("Feel free to type in commands");
    if env::var("INTERPRET").is_ok() {
        repl::start_interpreted()
    } else {
        repl::start()
    }
}
