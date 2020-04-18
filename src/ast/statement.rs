use super::{Expression, Identifier};
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expr(ExpressionStatement),
    Block(BlockStatement),
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::Let(stmt) => write!(f, "{}", stmt),
            Self::Return(stmt) => write!(f, "{}", stmt),
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

impl From<ReturnStatement> for Statement {
    fn from(stmt: ReturnStatement) -> Self {
        Self::Return(stmt)
    }
}

impl From<ExpressionStatement> for Statement {
    fn from(stmt: ExpressionStatement) -> Self {
        Self::Expr(stmt)
    }
}

#[cfg(test)]
impl Statement {
    pub fn pull_let(&self) -> &LetStatement {
        match self {
            Self::Let(stmt) => stmt,
            _ => panic!("expected let statement"),
        }
    }

    pub fn pull_return(&self) -> &ReturnStatement {
        match self {
            Self::Return(stmt) => stmt,
            _ => panic!("expected return statement"),
        }
    }

    pub fn pull_expr(&self) -> &ExpressionStatement {
        match self {
            Self::Expr(stmt) => stmt,
            _ => panic!("expected expr statement"),
        }
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
pub struct ReturnStatement {
    pub return_value: Expression,
}

impl Display for ReturnStatement {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "return {};", self.return_value)
    }
}

// ExpressionStatement, being just an Expression, does not have a
// token associated with it
#[derive(Debug, Clone, PartialEq)]
pub struct ExpressionStatement {
    pub expression: Expression,
}

impl Display for ExpressionStatement {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}", self.expression)
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
