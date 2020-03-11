use crate::token::{Token, TokenType};

pub struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    ch: u8,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let mut lexer = Self {
            input,
            position: 0,
            read_position: 0,
            ch: 0,
        };
        lexer.read_char();
        lexer
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = 0;
        } else {
            self.ch = self.input.as_bytes()[self.read_position];
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn next_token(&mut self) -> Token {
        let token = match self.ch {
            b'=' => Token::new_from_char(TokenType::Assign, self.ch),
            b';' => Token::new_from_char(TokenType::Semicolon, self.ch),
            b'(' => Token::new_from_char(TokenType::LParen, self.ch),
            b')' => Token::new_from_char(TokenType::RParen, self.ch),
            b',' => Token::new_from_char(TokenType::Comma, self.ch),
            b'+' => Token::new_from_char(TokenType::Plus, self.ch),
            b'{' => Token::new_from_char(TokenType::LBrace, self.ch),
            b'}' => Token::new_from_char(TokenType::RBrace, self.ch),
            0 => Token::new(TokenType::Eof, ""),
            _ => panic!("unrecognized char"),
        };
        self.read_char();
        token
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_next_token() {
        let input = "=+(){},;".to_owned();

        let cases = [
            (TokenType::Assign, "="),
            (TokenType::Plus, "+"),
            (TokenType::LParen, "("),
            (TokenType::RParen, ")"),
            (TokenType::LBrace, "{"),
            (TokenType::RBrace, "}"),
            (TokenType::Comma, ","),
            (TokenType::Semicolon, ";"),
            (TokenType::Eof, ""),
        ];

        let mut lexer = Lexer::new(input);

        for case in cases.iter() {
            let token = lexer.next_token();

            assert_eq!(token.token_type, case.0);
            assert_eq!(token.literal, case.1);
        }
    }
}
