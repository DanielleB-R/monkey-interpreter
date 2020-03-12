use crate::token::Token;
use std::rc::Rc;

pub trait Node {
    fn token_literal(&self) -> String;
}

#[derive(Debug, Clone)]
pub enum Statement {
    Let(LetStatement),
}

impl Node for Statement {
    fn token_literal(&self) -> String {
        match self {
            Self::Let(stmt) => stmt.token_literal(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expression {
    Identifier(Identifier),
}

impl Node for Expression {
    fn token_literal(&self) -> String {
        match self {
            Self::Identifier(expr) => expr.token_literal(),
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl Node for Program {
    fn token_literal(&self) -> String {
        self.statements
            .get(0)
            .map(|s| s.token_literal())
            .unwrap_or_else(|| "".to_owned())
    }
}

#[derive(Debug, Clone)]
pub struct LetStatement {
    pub token: Token,
    pub name: Rc<Identifier>,
    // pub value: Expression,
}

impl Node for LetStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

#[derive(Debug, Clone)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl Node for Identifier {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl From<Token> for Identifier {
    fn from(token: Token) -> Self {
        let value = token.literal.clone();
        Self { token, value }
    }
}
