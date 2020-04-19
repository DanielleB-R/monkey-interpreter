use super::{Expression, Identifier};
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Let(LetStatement),
    Return(Expression),
    Expr(Expression),
    Block(BlockStatement),
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::Let(stmt) => write!(f, "{}", stmt),
            Self::Return(expr) => write!(f, "return {};", expr),
            Self::Expr(stmt) => write!(f, "{}", stmt),
            Self::Block(stmt) => write!(f, "{}", stmt),
        }
    }
}

impl From<LetStatement> for Statement {
    fn from(stmt: LetStatement) -> Self {
        Self::Let(stmt)
    }
}

impl From<Expression> for Statement {
    fn from(expression: Expression) -> Self {
        Self::Expr(expression)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct LetStatement {
    pub name: Identifier,
    pub value: Expression,
}

impl Display for LetStatement {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "let {} = {};", self.name, self.value)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BlockStatement {
    pub statements: Vec<Statement>,
}

impl Display for BlockStatement {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        for stmt in self.statements.iter() {
            write!(f, "{}", stmt)?;
        }
        Ok(())
    }
}
