use crate::token::{Token, TokenType};

pub struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    ch: u8,
}

fn is_letter(c: u8) -> bool {
    c.is_ascii_alphabetic() || c == b'_'
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        Some(self.next_token())
    }
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
        self.ch = *self.input.as_bytes().get(self.read_position).unwrap_or(&0);
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn peek_char(&self) -> u8 {
        *self.input.as_bytes().get(self.read_position).unwrap_or(&0)
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let token = match self.ch {
            b'=' => {
                if self.peek_char() == b'=' {
                    let ch = self.ch;
                    self.read_char();
                    Token {
                        token_type: TokenType::Eq,
                        literal: String::from_utf8(vec![ch, self.ch]).unwrap(),
                    }
                } else {
                    Token::new_from_char(TokenType::Assign, self.ch)
                }
            }
            b'+' => Token::new_from_char(TokenType::Plus, self.ch),
            b'-' => Token::new_from_char(TokenType::Minus, self.ch),
            b'*' => Token::new_from_char(TokenType::Asterisk, self.ch),
            b'/' => Token::new_from_char(TokenType::Slash, self.ch),
            b'!' => {
                if self.peek_char() == b'=' {
                    let ch = self.ch;
                    self.read_char();
                    Token {
                        token_type: TokenType::NotEq,
                        literal: String::from_utf8(vec![ch, self.ch]).unwrap(),
                    }
                } else {
                    Token::new_from_char(TokenType::Bang, self.ch)
                }
            }
            b'<' => Token::new_from_char(TokenType::LT, self.ch),
            b'>' => Token::new_from_char(TokenType::GT, self.ch),
            b';' => Token::new_from_char(TokenType::Semicolon, self.ch),
            b'(' => Token::new_from_char(TokenType::LParen, self.ch),
            b')' => Token::new_from_char(TokenType::RParen, self.ch),
            b',' => Token::new_from_char(TokenType::Comma, self.ch),
            b'{' => Token::new_from_char(TokenType::LBrace, self.ch),
            b'}' => Token::new_from_char(TokenType::RBrace, self.ch),
            0 => Token {
                token_type: TokenType::Eof,
                literal: "".to_owned(),
            },
            c => {
                if is_letter(c) {
                    return Token::new(self.read_identifier());
                } else if c.is_ascii_digit() {
                    return Token {
                        token_type: TokenType::Int,
                        literal: self.read_number().to_owned(),
                    };
                } else {
                    Token::new_from_char(TokenType::Illegal, c)
                }
            }
        };
        self.read_char();
        token
    }

    fn read_identifier(&mut self) -> &str {
        let start = self.position;
        while is_letter(self.ch) {
            self.read_char()
        }
        &self.input[start..self.position]
    }

    fn read_number(&mut self) -> &str {
        let start = self.position;
        while self.ch.is_ascii_digit() {
            self.read_char()
        }
        &self.input[start..self.position]
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_ascii_whitespace() {
            self.read_char();
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_next_token() {
        let input = "let five = 5;
let ten = 10;

let add = fn(x, y) {
  x + y;
};

let result = add(five, ten);
!-/*5;
5 < 10 > 5;

if (5 < 10) {
    return true;
} else {
    return false;
}

10 == 10;
10 != 9;
"
        .to_owned();

        let cases = [
            (TokenType::Let, "let"),
            (TokenType::Ident, "five"),
            (TokenType::Assign, "="),
            (TokenType::Int, "5"),
            (TokenType::Semicolon, ";"),
            (TokenType::Let, "let"),
            (TokenType::Ident, "ten"),
            (TokenType::Assign, "="),
            (TokenType::Int, "10"),
            (TokenType::Semicolon, ";"),
            (TokenType::Let, "let"),
            (TokenType::Ident, "add"),
            (TokenType::Assign, "="),
            (TokenType::Function, "fn"),
            (TokenType::LParen, "("),
            (TokenType::Ident, "x"),
            (TokenType::Comma, ","),
            (TokenType::Ident, "y"),
            (TokenType::RParen, ")"),
            (TokenType::LBrace, "{"),
            (TokenType::Ident, "x"),
            (TokenType::Plus, "+"),
            (TokenType::Ident, "y"),
            (TokenType::Semicolon, ";"),
            (TokenType::RBrace, "}"),
            (TokenType::Semicolon, ";"),
            (TokenType::Let, "let"),
            (TokenType::Ident, "result"),
            (TokenType::Assign, "="),
            (TokenType::Ident, "add"),
            (TokenType::LParen, "("),
            (TokenType::Ident, "five"),
            (TokenType::Comma, ","),
            (TokenType::Ident, "ten"),
            (TokenType::RParen, ")"),
            (TokenType::Semicolon, ";"),
            (TokenType::Bang, "!"),
            (TokenType::Minus, "-"),
            (TokenType::Slash, "/"),
            (TokenType::Asterisk, "*"),
            (TokenType::Int, "5"),
            (TokenType::Semicolon, ";"),
            (TokenType::Int, "5"),
            (TokenType::LT, "<"),
            (TokenType::Int, "10"),
            (TokenType::GT, ">"),
            (TokenType::Int, "5"),
            (TokenType::Semicolon, ";"),
            (TokenType::If, "if"),
            (TokenType::LParen, "("),
            (TokenType::Int, "5"),
            (TokenType::LT, "<"),
            (TokenType::Int, "10"),
            (TokenType::RParen, ")"),
            (TokenType::LBrace, "{"),
            (TokenType::Return, "return"),
            (TokenType::True, "true"),
            (TokenType::Semicolon, ";"),
            (TokenType::RBrace, "}"),
            (TokenType::Else, "else"),
            (TokenType::LBrace, "{"),
            (TokenType::Return, "return"),
            (TokenType::False, "false"),
            (TokenType::Semicolon, ";"),
            (TokenType::RBrace, "}"),
            (TokenType::Int, "10"),
            (TokenType::Eq, "=="),
            (TokenType::Int, "10"),
            (TokenType::Semicolon, ";"),
            (TokenType::Int, "10"),
            (TokenType::NotEq, "!="),
            (TokenType::Int, "9"),
            (TokenType::Semicolon, ";"),
            (TokenType::Eof, ""),
        ]
        .iter()
        .map(|(token_type, literal)| Token {
            token_type: *token_type,
            literal: (*literal).to_owned(),
        });

        let lexer = Lexer::new(input);

        for (case, token) in cases.zip(lexer) {
            assert_eq!(token, case);
        }
    }
}
