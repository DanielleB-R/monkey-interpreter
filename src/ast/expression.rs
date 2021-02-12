use super::{statement::BlockStatement, Identifier};
use crate::token::Token;
use derive_more::Display;
use std::fmt::{Display, Formatter};
use strum_macros;

#[derive(Display, Debug, Clone, PartialEq)]
pub enum Expression {
    Identifier(Identifier),
    IntegerLiteral(i64),
    Boolean(Boolean),
    Prefix(PrefixExpression),
    Infix(InfixExpression),
    If(IfExpression),
    Function(FunctionLiteral),
    Call(CallExpression),
    Array(ArrayLiteral),
    String(String),
    Index(IndexExpression),
    Hash(HashLiteral),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, strum_macros::Display)]
pub enum Operator {
    #[strum(to_string = "!")]
    Bang,
    #[strum(to_string = "-")]
    Minus,
    #[strum(to_string = "+")]
    Plus,
    #[strum(to_string = "*")]
    Asterisk,
    #[strum(to_string = "/")]
    Slash,
    #[strum(to_string = "<")]
    LT,
    #[strum(to_string = ">")]
    GT,
    #[strum(to_string = "==")]
    Eq,
    #[strum(to_string = "!=")]
    NotEq,
}

impl From<Token> for Operator {
    fn from(input: Token) -> Self {
        match input {
            Token::Bang => Self::Bang,
            Token::Minus => Self::Minus,
            Token::Plus => Self::Plus,
            Token::Asterisk => Self::Asterisk,
            Token::Slash => Self::Slash,
            Token::LT => Self::LT,
            Token::GT => Self::GT,
            Token::Eq => Self::Eq,
            Token::NotEq => Self::NotEq,
            _ => panic!("invalid operator token"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct PrefixExpression {
    pub operator: Operator,
    pub right: Box<Expression>,
}

impl Display for PrefixExpression {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "({}{})", self.operator, self.right)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct InfixExpression {
    pub left: Box<Expression>,
    pub operator: Operator,
    pub right: Box<Expression>,
}

impl Display for InfixExpression {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "({} {} {})", self.left, self.operator, self.right)
    }
}

#[derive(Debug, Clone, PartialEq, Display)]
pub struct Boolean(pub bool);

impl From<Token> for Boolean {
    fn from(token: Token) -> Self {
        Self(match token {
            Token::True => true,
            Token::False => false,
            _ => panic!("converting non-boolean token to boolean expr"),
        })
    }
}

impl From<bool> for Boolean {
    fn from(value: bool) -> Self {
        Self(value)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfExpression {
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

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionLiteral {
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
    pub name: Option<String>,
}

impl Display for FunctionLiteral {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        let identifier_names: Vec<String> =
            self.parameters.iter().map(Identifier::to_string).collect();

        let preamble = match &self.name {
            Some(name) => format!("fn<{}>", name),
            None => "fn".to_owned(),
        };

        write!(
            f,
            "{}({}) {}",
            preamble,
            identifier_names.join(", "),
            self.body
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct CallExpression {
    pub function: Box<Expression>,
    pub arguments: Vec<Expression>,
}

impl Display for CallExpression {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        let argument_names: Vec<String> =
            self.arguments.iter().map(Expression::to_string).collect();

        write!(f, "{}({})", self.function, argument_names.join(", "))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArrayLiteral {
    pub elements: Vec<Expression>,
}

impl Display for ArrayLiteral {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        let argument_names: Vec<String> = self.elements.iter().map(Expression::to_string).collect();

        write!(f, "[{}]", argument_names.join(", "))
    }
}

impl From<Vec<Expression>> for ArrayLiteral {
    fn from(elements: Vec<Expression>) -> Self {
        Self { elements }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct IndexExpression {
    pub left: Box<Expression>,
    pub index: Box<Expression>,
}

impl Display for IndexExpression {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "({}[{}])", self.left, self.index)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct HashLiteral {
    pub pairs: Vec<(Expression, Expression)>,
}

impl Display for HashLiteral {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        let argument_names: Vec<String> = self
            .pairs
            .iter()
            .map(|(key, value)| format!("{}:{}", key, value))
            .collect();
        write!(f, "{{{}}}", argument_names.join(", "))
    }
}

impl From<Vec<(Expression, Expression)>> for HashLiteral {
    fn from(pairs: Vec<(Expression, Expression)>) -> Self {
        Self { pairs }
    }
}
