#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    Illegal,
    Eof,

    // Identifiers and literals
    Ident,
    Int,

    // Operators
    Assign,
    Plus,

    // Delimiters
    Comma,
    Semicolon,

    LParen,
    RParen,
    LBrace,
    RBrace,

    // Keywords
    Function,
    Let,
}

pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
}

impl Token {
    pub fn new(token_type: TokenType, literal: &str) -> Self {
        Self {
            token_type,
            literal: literal.to_owned(),
        }
    }

    pub fn new_from_char(token_type: TokenType, ch: u8) -> Self {
        Self {
            token_type,
            literal: String::from_utf8(vec![ch]).unwrap(),
        }
    }
}
