use crate::token::Token;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

pub trait Node: Display {
    fn token_literal(&self) -> String;
}

#[derive(Debug, Clone)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expr(ExpressionStatement),
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::Let(stmt) => write!(f, "{}", stmt),
            Self::Return(stmt) => write!(f, "{}", stmt),
            Self::Expr(stmt) => write!(f, "{}", stmt),
        }
    }
}

impl Node for Statement {
    fn token_literal(&self) -> String {
        match self {
            Self::Let(stmt) => stmt.token_literal(),
            Self::Return(stmt) => stmt.token_literal(),
            Self::Expr(stmt) => stmt.token_literal(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expression {
    Identifier(Identifier),
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::Identifier(expr) => write!(f, "{}", expr),
        }
    }
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

impl Display for Program {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for stmt in self.statements.iter() {
            write!(f, "{}", stmt)?;
        }
        Ok(())
    }
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

impl Display for LetStatement {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{} {} = {};", self.token_literal(), self.name, "")
    }
}

impl Node for LetStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

#[derive(Debug, Clone)]
pub struct ReturnStatement {
    pub token: Token,
    // pub return_value: Expression,
}

impl Display for ReturnStatement {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{} {};", self.token_literal(), "")
    }
}

impl Node for ReturnStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

#[derive(Debug, Clone)]
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Expression,
}

impl Display for ExpressionStatement {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "")
    }
}

impl Node for ExpressionStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

#[derive(Debug, Clone)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl Display for Identifier {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
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

#[cfg(test)]
mod test {
    use super::*;
    use crate::token::TokenType;

    #[test]
    fn test_display() {
        let program = Program {
            statements: vec![Statement::Let(LetStatement {
                token: Token {
                    token_type: TokenType::Let,
                    literal: "let".to_owned(),
                },
                name: Rc::new(
                    Token {
                        token_type: TokenType::Ident,
                        literal: "myVar".to_owned(),
                    }
                    .into(),
                ),
            })],
        };

        assert_eq!(format!("{}", program), "let myVar = ;");
    }
}
