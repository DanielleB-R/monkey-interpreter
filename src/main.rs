use monkey_interpreter::repl;
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
