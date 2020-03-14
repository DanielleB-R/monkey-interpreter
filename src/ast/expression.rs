use super::{statement::BlockStatement, Identifier, Node};
use crate::token::{Token, TokenType};
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone)]
pub enum Expression {
    Identifier(Identifier),
    IntegerLiteral(IntegerLiteral),
    Boolean(Boolean),
    Prefix(PrefixExpression),
    Infix(InfixExpression),
    If(IfExpression),
    Function(FunctionLiteral),
    Nil,
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::Identifier(expr) => write!(f, "{}", expr),
            Self::IntegerLiteral(expr) => write!(f, "{}", expr),
            Self::Prefix(expr) => write!(f, "{}", expr),
            Self::Infix(expr) => write!(f, "{}", expr),
            Self::Boolean(expr) => write!(f, "{}", expr),
            Self::If(expr) => write!(f, "{}", expr),
            Self::Function(expr) => write!(f, "{}", expr),
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
            Self::Boolean(expr) => expr.token(),
            Self::If(expr) => expr.token(),
            Self::Function(expr) => expr.token(),
            Self::Nil => panic!(),
        }
    }
}

impl Expression {
    pub fn pull_prefix(&self) -> &PrefixExpression {
        match self {
            Self::Prefix(expr) => expr,
            _ => panic!("expected prefix expression"),
        }
    }

    pub fn pull_integer(&self) -> &IntegerLiteral {
        match self {
            Self::IntegerLiteral(expr) => expr,
            _ => panic!("expected integer expression"),
        }
    }

    pub fn pull_identifier(&self) -> &Identifier {
        match self {
            Self::Identifier(expr) => expr,
            _ => panic!("expected identifier expression"),
        }
    }

    pub fn pull_boolean(&self) -> &Boolean {
        match self {
            Self::Boolean(expr) => expr,
            _ => panic!("expected boolean expression"),
        }
    }

    pub fn pull_infix(&self) -> &InfixExpression {
        match self {
            Self::Infix(expr) => expr,
            _ => panic!("expected infix expression"),
        }
    }

    pub fn pull_if(&self) -> &IfExpression {
        match self {
            Self::If(expr) => expr,
            _ => panic!("expected if expression"),
        }
    }
    pub fn pull_function(&self) -> &FunctionLiteral {
        match self {
            Self::Function(expr) => expr,
            _ => panic!("expected function expression"),
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

#[derive(Debug, Clone)]
pub struct Boolean {
    pub token: Token,
    pub value: bool,
}

impl Display for Boolean {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl Node for Boolean {
    fn token(&self) -> &Token {
        &self.token
    }
}

impl From<Token> for Boolean {
    fn from(token: Token) -> Self {
        let value = match token.token_type {
            TokenType::True => true,
            TokenType::False => false,
            _ => panic!("converting non-boolean token to boolean expr"),
        };
        Self { token, value }
    }
}

#[derive(Debug, Clone)]
pub struct IfExpression {
    pub token: Token,
    pub condition: Box<Expression>,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

impl Display for IfExpression {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "if{} {}", self.condition, self.consequence)?;
        if let Some(alt) = &self.alternative {
            write!(f, "else {}", alt)?;
        }
        Ok(())
    }
}

impl Node for IfExpression {
    fn token(&self) -> &Token {
        &self.token
    }
}

#[derive(Debug, Clone)]
pub struct FunctionLiteral {
    pub token: Token,
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
}

impl Display for FunctionLiteral {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        let identifier_names: Vec<String> =
            self.parameters.iter().map(Identifier::to_string).collect();

        write!(f, "fn({}) {}", identifier_names.join(", "), self.body)
    }
}

impl Node for FunctionLiteral {
    fn token(&self) -> &Token {
        &self.token
    }
}
