use crate::lexer::Lexer;
use crate::token::TokenType;
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

        let mut lexer = Lexer::new(line);

        loop {
            let token = lexer.next_token();
            if token.token_type == TokenType::Eof {
                break;
            }
            println!("{:?}", token);
        }
    }
}
