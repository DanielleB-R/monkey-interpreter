use super::{statement::BlockStatement, Identifier};
use crate::token::Token;
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, PartialEq)]
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
            Self::Call(expr) => write!(f, "{}", expr),
            Self::Array(expr) => write!(f, "{}", expr),
            Self::String(expr) => write!(f, "{}", expr),
            Self::Index(expr) => write!(f, "{}", expr),
            Self::Hash(expr) => write!(f, "{}", expr),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Operator {
    Bang,
    Minus,
    Plus,
    Asterisk,
    Slash,
    LT,
    GT,
    Eq,
    NotEq,
}

impl Display for Operator {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Bang => "!",
                Self::Minus => "-",
                Self::Plus => "+",
                Self::Asterisk => "*",
                Self::Slash => "/",
                Self::LT => "<",
                Self::GT => ">",
                Self::Eq => "==",
                Self::NotEq => "!=",
            }
        )
    }
}

impl From<&str> for Operator {
    fn from(input: &str) -> Self {
        match input {
            "!" => Self::Bang,
            "-" => Self::Minus,
            "+" => Self::Plus,
            "*" => Self::Asterisk,
            "/" => Self::Slash,
            "<" => Self::LT,
            ">" => Self::GT,
            "==" => Self::Eq,
            "!=" => Self::NotEq,
            _ => panic!("invalid operator string"),
        }
    }
}

impl From<&Token> for Operator {
    fn from(input: &Token) -> Self {
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
            _ => panic!("invalid operator string"),
        }
    }
}

impl From<Token> for Operator {
    fn from(input: Token) -> Self {
        (&input).into()
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

#[derive(Debug, Clone, PartialEq)]
pub struct Boolean {
    pub value: bool,
}

impl Display for Boolean {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl From<Token> for Boolean {
    fn from(token: Token) -> Self {
        Self {
            value: match token {
                Token::True => true,
                Token::False => false,
                _ => panic!("converting non-boolean token to boolean expr"),
            },
        }
    }
}

impl From<bool> for Boolean {
    fn from(value: bool) -> Self {
        Self { value }
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
