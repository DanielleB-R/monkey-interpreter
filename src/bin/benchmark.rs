use monkey_interpreter::{evaluator::eval, Compiler, Environment, Lexer, Parser, VM};
use std::env;
use std::time::Instant;

static input: &str = "let fibonacci = fn(x) {
if (x == 0) { 0
     } else {
       if (x == 1) {
         return 1;
       } else {
         fibonacci(x - 1) + fibonacci(x - 2);
       }
} };
   fibonacci(35);";

fn main() {
    let program = Parser::new(Lexer::new(input.to_owned()))
        .parse_program()
        .expect("Parse errors found");

    let start;
    let duration;
    let result;

    let engine = env::args().nth(1).unwrap();
    if engine == "vm" {
        let mut comp = Compiler::default();
        comp.compile(program).unwrap();

        let mut machine = VM::new(comp.bytecode());

        start = Instant::now();

        machine.run().unwrap();

        duration = start.elapsed();
        result = machine.last_popped_stack_element().clone();
    } else {
        let mut env = Environment::new();
        start = Instant::now();
        result = eval(program, &mut env).unwrap();
        duration = start.elapsed();
    }

    println!(
        "engine={}, result={}, duration={}",
        engine,
        result,
        duration.as_secs_f64(),
    );
}
