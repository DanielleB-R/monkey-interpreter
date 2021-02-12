use crate::token::Token;

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
        self.ch = self
            .input
            .as_bytes()
            .get(self.read_position)
            .copied()
            .unwrap_or(0);
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn peek_char(&self) -> u8 {
        self.input
            .as_bytes()
            .get(self.read_position)
            .copied()
            .unwrap_or(0)
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let token = match self.ch {
            b'=' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    Token::Eq
                } else {
                    Token::Assign
                }
            }
            b'+' => Token::Plus,
            b'-' => Token::Minus,
            b'*' => Token::Asterisk,
            b'/' => Token::Slash,
            b'!' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    Token::NotEq
                } else {
                    Token::Bang
                }
            }
            b'<' => Token::LT,
            b'>' => Token::GT,
            b';' => Token::Semicolon,
            b'(' => Token::LParen,
            b')' => Token::RParen,
            b',' => Token::Comma,
            b':' => Token::Colon,
            b'{' => Token::LBrace,
            b'}' => Token::RBrace,
            b'[' => Token::LBracket,
            b']' => Token::RBracket,
            b'"' => Token::String(self.read_string().to_owned()),
            0 => Token::Eof,
            c => {
                if is_letter(c) {
                    return self.read_identifier().into();
                } else if c.is_ascii_digit() {
                    return Token::Int(self.read_number().to_owned());
                } else {
                    Token::Illegal(c)
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

    fn read_string(&mut self) -> &str {
        let position = self.position + 1;
        loop {
            self.read_char();

            if self.ch == b'"' || self.ch == 0 {
                break;
            }
        }
        &self.input[position..self.position]
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
\"foobar\"
\"foo bar\"
[1, 2];
{\"foo\": \"bar\"}
"
        .to_owned();

        let expected = vec![
            Token::Let,
            Token::Ident("five".to_owned()),
            Token::Assign,
            Token::Int("5".to_owned()),
            Token::Semicolon,
            Token::Let,
            Token::Ident("ten".to_owned()),
            Token::Assign,
            Token::Int("10".to_owned()),
            Token::Semicolon,
            Token::Let,
            Token::Ident("add".to_owned()),
            Token::Assign,
            Token::Function,
            Token::LParen,
            Token::Ident("x".to_owned()),
            Token::Comma,
            Token::Ident("y".to_owned()),
            Token::RParen,
            Token::LBrace,
            Token::Ident("x".to_owned()),
            Token::Plus,
            Token::Ident("y".to_owned()),
            Token::Semicolon,
            Token::RBrace,
            Token::Semicolon,
            Token::Let,
            Token::Ident("result".to_owned()),
            Token::Assign,
            Token::Ident("add".to_owned()),
            Token::LParen,
            Token::Ident("five".to_owned()),
            Token::Comma,
            Token::Ident("ten".to_owned()),
            Token::RParen,
            Token::Semicolon,
            Token::Bang,
            Token::Minus,
            Token::Slash,
            Token::Asterisk,
            Token::Int("5".to_owned()),
            Token::Semicolon,
            Token::Int("5".to_owned()),
            Token::LT,
            Token::Int("10".to_owned()),
            Token::GT,
            Token::Int("5".to_owned()),
            Token::Semicolon,
            Token::If,
            Token::LParen,
            Token::Int("5".to_owned()),
            Token::LT,
            Token::Int("10".to_owned()),
            Token::RParen,
            Token::LBrace,
            Token::Return,
            Token::True,
            Token::Semicolon,
            Token::RBrace,
            Token::Else,
            Token::LBrace,
            Token::Return,
            Token::False,
            Token::Semicolon,
            Token::RBrace,
            Token::Int("10".to_owned()),
            Token::Eq,
            Token::Int("10".to_owned()),
            Token::Semicolon,
            Token::Int("10".to_owned()),
            Token::NotEq,
            Token::Int("9".to_owned()),
            Token::Semicolon,
            Token::String("foobar".to_owned()),
            Token::String("foo bar".to_owned()),
            Token::LBracket,
            Token::Int("1".to_owned()),
            Token::Comma,
            Token::Int("2".to_owned()),
            Token::RBracket,
            Token::Semicolon,
            Token::LBrace,
            Token::String("foo".to_owned()),
            Token::Colon,
            Token::String("bar".to_owned()),
            Token::RBrace,
            Token::Eof,
        ];

        let lexer = Lexer::new(input);

        for (exp, token) in expected.into_iter().zip(lexer) {
            assert_eq!(token, exp);
        }
    }
}
