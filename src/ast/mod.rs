mod expression;
mod statement;
pub use expression::*;
pub use statement::*;

use std::fmt::{Display, Formatter};

#[derive(Debug, Clone)]
pub enum Node {
    Program(Program),
    Statement(Statement),
    Expression(Expression),
}

impl From<Program> for Node {
    fn from(program: Program) -> Self {
        Self::Program(program)
    }
}

impl From<Statement> for Node {
    fn from(statement: Statement) -> Self {
        Self::Statement(statement)
    }
}

impl From<Expression> for Node {
    fn from(expression: Expression) -> Self {
        Self::Expression(expression)
    }
}

impl From<BlockStatement> for Node {
    fn from(block: BlockStatement) -> Self {
        Statement::Block(block).into()
    }
}

#[derive(Debug, Clone, Default)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl Display for Program {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        for stmt in &self.statements {
            write!(f, "{}", stmt)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Identifier {
    pub value: String,
}

impl Display for Identifier {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl From<String> for Identifier {
    fn from(value: String) -> Self {
        Self { value }
    }
}

impl From<&str> for Identifier {
    fn from(value: &str) -> Self {
        Self {
            value: value.to_owned(),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_display() {
        let program = Program {
            statements: vec![Statement::Let(LetStatement {
                name: "myVar".to_owned().into(),
                value: Expression::Identifier(Identifier {
                    value: "anotherVar".to_owned(),
                }),
            })],
        };

        assert_eq!(format!("{}", program), "let myVar = anotherVar;");
    }
}
