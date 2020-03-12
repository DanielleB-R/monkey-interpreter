mod expression;
mod statement;
pub use expression::*;
pub use statement::*;

use crate::token::Token;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

pub trait Node: Display {
    fn token(&self) -> &Token;
    fn token_literal(&self) -> String {
        self.token().literal.clone()
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
    fn token(&self) -> &Token {
        self.statements.get(0).unwrap().token()
    }

    fn token_literal(&self) -> String {
        self.statements
            .get(0)
            .map(|s| s.token_literal())
            .unwrap_or_else(|| "".to_owned())
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
    fn token(&self) -> &Token {
        &self.token
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
                value: Expression::Identifier(Identifier {
                    token: Token {
                        token_type: TokenType::Ident,
                        literal: "anotherVar".to_owned(),
                    },
                    value: "anotherVar".to_owned(),
                }),
            })],
        };

        assert_eq!(format!("{}", program), "let myVar = anotherVar;");
    }
}
