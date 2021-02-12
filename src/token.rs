use strum_macros::{Display, EnumDiscriminants};

#[derive(Debug, Display, Clone, PartialEq, Eq, EnumDiscriminants)]
#[strum_discriminants(derive(Hash, Display))]
#[strum_discriminants(name(TokenType))]
pub enum Token {
    Illegal(u8),
    Eof,

    // Identifiers and literals
    Ident(String),
    Int(String),
    String(String),

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
    Colon,

    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,

    // Keywords
    Else,
    False,
    Function,
    If,
    Let,
    Return,
    True,
}

impl From<&str> for Token {
    fn from(text: &str) -> Self {
        match text {
            "let" => Self::Let,
            "fn" => Self::Function,
            "if" => Self::If,
            "return" => Self::Return,
            "true" => Self::True,
            "else" => Self::Else,
            "false" => Self::False,
            identifier => Self::Ident(identifier.to_owned()),
        }
    }
}

impl Token {
    pub fn is(&self, token_type: TokenType) -> bool {
        TokenType::from(self) == token_type
    }
}
