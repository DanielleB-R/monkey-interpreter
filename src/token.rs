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
    Minus,
    Slash,
    Asterisk,
    Bang,
    LT,
    GT,
    Eq,
    NotEq,

    // Delimiters
    Comma,
    Semicolon,

    LParen,
    RParen,
    LBrace,
    RBrace,

    // Keywords
    Else,
    False,
    Function,
    If,
    Let,
    Return,
    True,
}

impl TokenType {
    pub fn type_for_name(name: &str) -> Self {
        // TODO Realize with a hashmap
        match name {
            "let" => Self::Let,
            "fn" => Self::Function,
            "if" => Self::If,
            "return" => Self::Return,
            "true" => Self::True,
            "else" => Self::Else,
            "false" => Self::False,
            _ => Self::Ident,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
}

impl Token {
    pub fn new(literal: &str) -> Self {
        Self {
            token_type: TokenType::type_for_name(literal),
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
