use super::{Identifier, Node};
use crate::token::Token;
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone)]
pub enum Expression {
    Identifier(Identifier),
    IntegerLiteral(IntegerLiteral),
    Prefix(PrefixExpression),
    Infix(InfixExpression),
    Nil,
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::Identifier(expr) => write!(f, "{}", expr),
            Self::IntegerLiteral(expr) => write!(f, "{}", expr),
            Self::Prefix(expr) => write!(f, "{}", expr),
            Self::Infix(expr) => write!(f, "{}", expr),
            Self::Nil => write!(f, ""),
        }
    }
}

impl Node for Expression {
    fn token(&self) -> &Token {
        match self {
            Self::Identifier(expr) => expr.token(),
            Self::IntegerLiteral(expr) => expr.token(),
            Self::Prefix(expr) => expr.token(),
            Self::Infix(expr) => expr.token(),
            Self::Nil => panic!(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

impl Display for IntegerLiteral {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}", self.token.literal)
    }
}

impl Node for IntegerLiteral {
    fn token(&self) -> &Token {
        &self.token
    }
}

#[derive(Debug, Clone)]
pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Box<Expression>,
}

impl Display for PrefixExpression {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "({}{})", self.operator, self.right)
    }
}

impl Node for PrefixExpression {
    fn token(&self) -> &Token {
        &self.token
    }
}

#[derive(Debug, Clone)]
pub struct InfixExpression {
    pub token: Token,
    pub left: Box<Expression>,
    pub operator: String,
    pub right: Box<Expression>,
}

impl Display for InfixExpression {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "({} {} {})", self.left, self.operator, self.right)
    }
}

impl Node for InfixExpression {
    fn token(&self) -> &Token {
        &self.token
    }
}
