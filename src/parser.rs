use crate::ast::{self, Expression, Statement};
use crate::lexer::Lexer;
use crate::token::{Token, TokenType};
use custom_error::custom_error;

custom_error! {
    #[derive(Clone, PartialEq, Eq)]
    pub ParseError

    InvalidInteger{literal: String} = "could not parse {literal} as integer",
    WrongNextToken{expected: TokenType, actual: TokenType} = "expected next token to be {expected}, got {actual} instead",
    MissingPrefixParseFunction{token_type: TokenType} = "no prefix parse function for {token_type} found",
}

type InfixParseFn = fn(&mut Parser, Expression) -> Result<Expression, ParseError>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
    Index,
}

impl From<&Token> for Precedence {
    fn from(t: &Token) -> Self {
        match t {
            Token::Eq => Self::Equals,
            Token::NotEq => Self::Equals,
            Token::LT => Self::LessGreater,
            Token::GT => Self::LessGreater,
            Token::Plus => Self::Sum,
            Token::Minus => Self::Sum,
            Token::Slash => Self::Product,
            Token::Asterisk => Self::Product,
            Token::LParen => Self::Call,
            Token::LBracket => Self::Index,
            _ => Self::Lowest,
        }
    }
}

pub struct Parser {
    lexer: std::iter::Peekable<Lexer>,
    errors: Vec<ParseError>,

    cur_token: Option<Token>,
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Self {
        Self {
            cur_token: Some(lexer.next().unwrap()),
            lexer: lexer.peekable(),
            errors: Default::default(),
        }
    }

    fn next_token(&mut self) {
        self.cur_token = Some(self.lexer.next().unwrap());
    }

    fn peek_token(&mut self) -> &Token {
        self.lexer.peek().unwrap()
    }

    fn cur_token_is(&self, token_type: TokenType) -> bool {
        self.cur_token.as_ref().unwrap().is(token_type)
    }

    fn cur_token_type(&self) -> TokenType {
        self.cur_token.as_ref().unwrap().into()
    }

    fn take_token(&mut self) -> Token {
        self.cur_token.take().unwrap()
    }

    fn advance_token(&mut self) -> Token {
        self.cur_token.replace(self.lexer.next().unwrap()).unwrap()
    }

    fn skip(&mut self, token_type: TokenType) {
        if self.peek_token().is(token_type) {
            self.next_token();
        }
    }

    pub fn parse_program(mut self) -> Result<ast::Program, Vec<ParseError>> {
        let mut program = ast::Program::default();

        while !self.cur_token_is(TokenType::Eof) {
            match self.parse_statement() {
                Ok(stmt) => program.statements.push(stmt),
                Err(err) => self.errors.push(err),
            }
            self.next_token();
        }

        if self.errors.is_empty() {
            Ok(program)
        } else {
            Err(self.errors)
        }
    }

    fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        match self.cur_token_type() {
            TokenType::Let => self.parse_let_statement(),
            TokenType::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Result<Statement, ParseError> {
        self.expect_peek(TokenType::Ident)?;

        let name = self.take_token().into();
        self.expect_peek(TokenType::Assign)?;
        self.next_token();

        let value = self.parse_expression(Precedence::Lowest)?;

        self.skip(TokenType::Semicolon);

        Ok(ast::LetStatement { name, value }.into())
    }

    fn parse_return_statement(&mut self) -> Result<Statement, ParseError> {
        self.next_token();

        let return_value = self.parse_expression(Precedence::Lowest)?;

        self.skip(TokenType::Semicolon);

        Ok(ast::ReturnStatement { return_value }.into())
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, ParseError> {
        let expression = self.parse_expression(Precedence::Lowest);

        self.skip(TokenType::Semicolon);

        Ok((expression?).into())
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, ParseError> {
        let mut left = self.parse_prefix()?;

        while !self.peek_token().is(TokenType::Semicolon) && precedence < self.peek_token().into() {
            let infix = match self.parse_infix() {
                None => return Ok(left),
                Some(infix) => infix,
            };
            self.next_token();
            left = infix(self, left)?
        }

        Ok(left)
    }

    fn parse_prefix(&mut self) -> Result<Expression, ParseError> {
        match self.take_token() {
            Token::Ident(name) => self.parse_identifier(name),
            Token::Int(contents) => self.parse_integer_literal(contents),
            token @ Token::Bang | token @ Token::Minus => self.parse_prefix_expression(token),
            token @ Token::True | token @ Token::False => self.parse_boolean(token),
            Token::LParen => self.parse_grouped_expression(),
            Token::If => self.parse_if_expression(),
            Token::Function => self.parse_function_literal(),
            Token::String(literal) => self.parse_string_literal(literal),
            Token::LBracket => self.parse_array_literal(),
            Token::LBrace => self.parse_hash_literal(),
            token => {
                self.cur_token = Some(token);
                Err(ParseError::MissingPrefixParseFunction {
                    token_type: self.cur_token_type(),
                })
            }
        }
    }

    fn parse_infix(&mut self) -> Option<InfixParseFn> {
        match self.peek_token() {
            Token::Plus
            | Token::Minus
            | Token::Slash
            | Token::Asterisk
            | Token::Eq
            | Token::NotEq
            | Token::LT
            | Token::GT => Some(Self::parse_infix_expression),
            Token::LParen => Some(Self::parse_call_expression),
            Token::LBracket => Some(Self::parse_index_expression),
            _ => None,
        }
    }

    fn parse_identifier(&self, name: String) -> Result<Expression, ParseError> {
        Ok(Expression::Identifier(name.into()))
    }

    fn parse_integer_literal(&self, contents: String) -> Result<Expression, ParseError> {
        contents
            .parse()
            .map(Expression::IntegerLiteral)
            .map_err(|_| ParseError::InvalidInteger { literal: contents })
    }

    fn parse_boolean(&self, token: Token) -> Result<Expression, ParseError> {
        Ok(Expression::Boolean(token.into()))
    }

    fn expect_peek(&mut self, expected: TokenType) -> Result<(), ParseError> {
        let peek_token = self.peek_token();
        if !peek_token.is(expected) {
            return Err(ParseError::WrongNextToken {
                expected,
                actual: peek_token.into(),
            });
        }
        self.next_token();
        Ok(())
    }

    fn parse_prefix_expression(&mut self, token: Token) -> Result<Expression, ParseError> {
        self.next_token();
        self.parse_expression(Precedence::Prefix).map(|right| {
            Expression::Prefix(ast::PrefixExpression {
                operator: token.into(),
                right: Box::new(right),
            })
        })
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Result<Expression, ParseError> {
        let token = self.advance_token();
        self.parse_expression((&token).into()).map(|right| {
            Expression::Infix(ast::InfixExpression {
                operator: token.into(),
                left: Box::new(left),
                right: Box::new(right),
            })
        })
    }

    fn parse_grouped_expression(&mut self) -> Result<Expression, ParseError> {
        self.next_token();

        let exp = self.parse_expression(Precedence::Lowest)?;

        self.expect_peek(TokenType::RParen)?;
        Ok(exp)
    }

    fn parse_if_expression(&mut self) -> Result<Expression, ParseError> {
        self.expect_peek(TokenType::LParen)?;

        self.next_token();
        let condition = self.parse_expression(Precedence::Lowest)?;

        self.expect_peek(TokenType::RParen)?;
        self.expect_peek(TokenType::LBrace)?;

        let consequence = self.parse_block_statement();

        let alternative = if self.peek_token().is(TokenType::Else) {
            self.next_token();

            self.expect_peek(TokenType::LBrace)?;

            Some(self.parse_block_statement())
        } else {
            None
        };

        Ok(Expression::If(ast::IfExpression {
            condition: Box::new(condition),
            consequence,
            alternative,
        }))
    }

    fn parse_block_statement(&mut self) -> ast::BlockStatement {
        self.next_token();
        let mut statements = vec![];

        while !self.cur_token_is(TokenType::RBrace) && !self.cur_token_is(TokenType::Eof) {
            match self.parse_statement() {
                Ok(stmt) => statements.push(stmt),
                Err(err) => self.errors.push(err),
            }

            self.next_token();
        }

        ast::BlockStatement { statements }
    }

    fn parse_function_literal(&mut self) -> Result<Expression, ParseError> {
        self.expect_peek(TokenType::LParen)?;

        let parameters = self.parse_function_parameters()?;

        self.expect_peek(TokenType::LBrace)?;

        let body = self.parse_block_statement();
        Ok(Expression::Function(ast::FunctionLiteral {
            parameters,
            body,
        }))
    }

    fn parse_function_parameters(&mut self) -> Result<Vec<ast::Identifier>, ParseError> {
        let mut identifiers = Default::default();

        if self.peek_token().is(TokenType::RParen) {
            self.next_token();
            return Ok(identifiers);
        }

        self.next_token();

        identifiers.push(self.take_token().into());

        while self.peek_token().is(TokenType::Comma) {
            self.next_token();
            self.next_token();
            // TODO this panics on certain badly-formed input e.g. "fn (a,)"
            identifiers.push(self.take_token().into());
        }

        self.expect_peek(TokenType::RParen)?;
        Ok(identifiers)
    }

    fn parse_call_expression(&mut self, function: Expression) -> Result<Expression, ParseError> {
        self.parse_expression_list(TokenType::RParen)
            .map(|arguments| {
                Expression::Call(ast::CallExpression {
                    function: Box::new(function),
                    arguments,
                })
            })
    }

    fn parse_expression_list(&mut self, end: TokenType) -> Result<Vec<Expression>, ParseError> {
        let mut list = Default::default();

        if self.peek_token().is(end) {
            self.next_token();
            return Ok(list);
        }

        self.next_token();

        list.push(self.parse_expression(Precedence::Lowest)?);

        while self.peek_token().is(TokenType::Comma) {
            self.next_token();
            self.next_token();
            list.push(self.parse_expression(Precedence::Lowest)?);
        }

        self.expect_peek(end)?;
        Ok(list)
    }

    fn parse_string_literal(&self, literal: String) -> Result<Expression, ParseError> {
        Ok(Expression::String(literal))
    }

    fn parse_array_literal(&mut self) -> Result<Expression, ParseError> {
        let elements = self.parse_expression_list(TokenType::RBracket)?;
        Ok(Expression::Array(ast::ArrayLiteral { elements }))
    }

    fn parse_index_expression(&mut self, left: Expression) -> Result<Expression, ParseError> {
        self.next_token();

        let index = self.parse_expression(Precedence::Lowest)?;

        self.expect_peek(TokenType::RBracket)?;
        Ok(Expression::Index(ast::IndexExpression {
            left: Box::new(left),
            index: Box::new(index),
        }))
    }

    fn parse_hash_literal(&mut self) -> Result<Expression, ParseError> {
        let mut pairs = vec![];

        while !self.peek_token().is(TokenType::RBrace) {
            self.next_token();

            let key = self.parse_expression(Precedence::Lowest)?;

            self.expect_peek(TokenType::Colon)?;

            self.next_token();
            let value = self.parse_expression(Precedence::Lowest)?;
            pairs.push((key, value));

            if !self.peek_token().is(TokenType::RBrace) {
                self.expect_peek(TokenType::Comma)?;
            }
        }

        self.expect_peek(TokenType::RBrace)?;
        Ok(Expression::Hash(ast::HashLiteral { pairs }))
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::Operator;
    use crate::lexer::Lexer;

    #[test]
    fn test_let_statements() {
        let input = "
let x = 5;
let y = true;
let foobar = y;
"
        .to_owned();

        let program = Parser::new(Lexer::new(input))
            .parse_program()
            .expect("Parse errors found");

        assert_eq!(program.statements.len(), 3);

        let cases = vec![
            ("x", Expression::IntegerLiteral(5)),
            ("y", Expression::Boolean(true.into())),
            ("foobar", Expression::Identifier("y".into())),
        ];

        for ((name, value), stmt) in cases.into_iter().zip(program.statements.into_iter()) {
            assert_eq!(
                stmt,
                Statement::Let(ast::LetStatement {
                    name: name.into(),
                    value
                })
            );
        }
    }

    #[test]
    fn test_return_statements() {
        let input = "
return 5;
return true;
return foobar;
"
        .to_owned();
        let program = Parser::new(Lexer::new(input))
            .parse_program()
            .expect("Parse errors found");

        assert_eq!(
            program.statements,
            vec![
                Expression::IntegerLiteral(5),
                Expression::Boolean(true.into()),
                Expression::Identifier("foobar".into()),
            ]
            .into_iter()
            .map(|return_value| Statement::Return(ast::ReturnStatement { return_value }))
            .collect::<Vec<Statement>>()
        );
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;".to_owned();

        let program = Parser::new(Lexer::new(input))
            .parse_program()
            .expect("Parse errors found");

        assert_eq!(
            program.statements,
            vec![Expression::Identifier("foobar".into()).into()]
        )
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = "5;".to_owned();

        let program = Parser::new(Lexer::new(input))
            .parse_program()
            .expect("Parse errors found");

        assert_eq!(
            program.statements,
            vec![Expression::IntegerLiteral(5).into()]
        );
    }

    #[test]
    fn test_parsing_prefix_expressions() {
        let cases = vec![
            ("!5;", Operator::Bang, Expression::IntegerLiteral(5)),
            ("-15;", Operator::Minus, Expression::IntegerLiteral(15)),
            (
                "!foobar;",
                Operator::Bang,
                Expression::Identifier("foobar".into()),
            ),
            (
                "-foobar;",
                Operator::Minus,
                Expression::Identifier("foobar".into()),
            ),
            ("!true;", Operator::Bang, Expression::Boolean(true.into())),
            ("!false;", Operator::Bang, Expression::Boolean(false.into())),
        ];

        for (input, operator, value) in cases {
            let program = Parser::new(Lexer::new(input.to_owned()))
                .parse_program()
                .expect("Parse errors found");

            assert_eq!(
                program.statements,
                vec![Expression::Prefix(ast::PrefixExpression {
                    operator,
                    right: Box::new(value),
                })
                .into()]
            );
        }
    }

    #[test]
    fn test_parsing_infix_expressions() {
        let cases = vec![
            (
                "5 + 5;",
                Expression::IntegerLiteral(5),
                Operator::Plus,
                Expression::IntegerLiteral(5),
            ),
            (
                "5 - 5;",
                Expression::IntegerLiteral(5),
                Operator::Minus,
                Expression::IntegerLiteral(5),
            ),
            (
                "5 * 5;",
                Expression::IntegerLiteral(5),
                Operator::Asterisk,
                Expression::IntegerLiteral(5),
            ),
            (
                "5 / 5;",
                Expression::IntegerLiteral(5),
                Operator::Slash,
                Expression::IntegerLiteral(5),
            ),
            (
                "5 > 5;",
                Expression::IntegerLiteral(5),
                Operator::GT,
                Expression::IntegerLiteral(5),
            ),
            (
                "5 < 5;",
                Expression::IntegerLiteral(5),
                Operator::LT,
                Expression::IntegerLiteral(5),
            ),
            (
                "5 == 5;",
                Expression::IntegerLiteral(5),
                Operator::Eq,
                Expression::IntegerLiteral(5),
            ),
            (
                "5 != 5;",
                Expression::IntegerLiteral(5),
                Operator::NotEq,
                Expression::IntegerLiteral(5),
            ),
            (
                "foobar + barfoo;",
                Expression::Identifier("foobar".into()),
                Operator::Plus,
                Expression::Identifier("barfoo".into()),
            ),
            (
                "foobar - barfoo;",
                Expression::Identifier("foobar".into()),
                Operator::Minus,
                Expression::Identifier("barfoo".into()),
            ),
            (
                "foobar * barfoo;",
                Expression::Identifier("foobar".into()),
                Operator::Asterisk,
                Expression::Identifier("barfoo".into()),
            ),
            (
                "foobar / barfoo;",
                Expression::Identifier("foobar".into()),
                Operator::Slash,
                Expression::Identifier("barfoo".into()),
            ),
            (
                "foobar > barfoo;",
                Expression::Identifier("foobar".into()),
                Operator::GT,
                Expression::Identifier("barfoo".into()),
            ),
            (
                "foobar < barfoo;",
                Expression::Identifier("foobar".into()),
                Operator::LT,
                Expression::Identifier("barfoo".into()),
            ),
            (
                "foobar == barfoo;",
                Expression::Identifier("foobar".into()),
                Operator::Eq,
                Expression::Identifier("barfoo".into()),
            ),
            (
                "foobar != barfoo;",
                Expression::Identifier("foobar".into()),
                Operator::NotEq,
                Expression::Identifier("barfoo".into()),
            ),
            (
                "true == true",
                Expression::Boolean(true.into()),
                Operator::Eq,
                Expression::Boolean(true.into()),
            ),
            (
                "true != false",
                Expression::Boolean(true.into()),
                Operator::NotEq,
                Expression::Boolean(false.into()),
            ),
            (
                "false == false",
                Expression::Boolean(false.into()),
                Operator::Eq,
                Expression::Boolean(false.into()),
            ),
        ];

        for (input, left, operator, right) in cases {
            let program = Parser::new(Lexer::new(input.to_owned()))
                .parse_program()
                .expect("Parse errors found");

            assert_eq!(
                program.statements,
                vec![Expression::Infix(ast::InfixExpression {
                    left: Box::new(left),
                    operator,
                    right: Box::new(right),
                })
                .into()]
            );
        }
    }

    #[test]
    fn test_operator_precedence_parsing() {
        let cases = vec![
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            ("true", "true"),
            ("false", "false"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
            ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            (
                "add(a + b + c * d / f + g)",
                "add((((a + b) + ((c * d) / f)) + g))",
            ),
            (
                "a * [1 ,2, 3, 4][b * c] * d",
                "((a * ([1, 2, 3, 4][(b * c)])) * d)",
            ),
            (
                "add(a * b[2], b[1], 2 * [1, 2][1])",
                "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
            ),
        ];

        for (input, output) in cases {
            let program = Parser::new(Lexer::new(input.to_owned()))
                .parse_program()
                .expect("Parse errors found");

            assert_eq!(format!("{}", program), output);
        }
    }

    #[test]
    fn test_boolean_expression() {
        let cases = vec![
            ("true;", Expression::Boolean(true.into())),
            ("false;", Expression::Boolean(false.into())),
        ];

        for (input, expected) in cases {
            let program = Parser::new(Lexer::new(input.to_owned()))
                .parse_program()
                .expect("Parse errors found");

            assert_eq!(program.statements, vec![expected.into()]);
        }
    }

    #[test]
    fn test_if_expression() {
        let input = "if (x > y) { x }".to_owned();

        let program = Parser::new(Lexer::new(input.to_owned()))
            .parse_program()
            .expect("Parse errors found");

        assert_eq!(
            program.statements,
            vec![Expression::If(ast::IfExpression {
                condition: Box::new(Expression::Infix(ast::InfixExpression {
                    left: Box::new(Expression::Identifier("x".into())),
                    operator: Operator::GT,
                    right: Box::new(Expression::Identifier("y".into())),
                })),
                consequence: ast::BlockStatement {
                    statements: vec![Expression::Identifier("x".into()).into()],
                },
                alternative: None,
            })
            .into()]
        );
    }

    #[test]
    fn test_if_else_expression() {
        let input = "if (x > y) { x } else { y }".to_owned();

        let program = Parser::new(Lexer::new(input.to_owned()))
            .parse_program()
            .expect("Parse errors found");

        assert_eq!(
            program.statements,
            vec![Expression::If(ast::IfExpression {
                condition: Box::new(Expression::Infix(ast::InfixExpression {
                    left: Box::new(Expression::Identifier("x".into())),
                    operator: Operator::GT,
                    right: Box::new(Expression::Identifier("y".into())),
                })),
                consequence: ast::BlockStatement {
                    statements: vec![Expression::Identifier("x".into()).into()],
                },
                alternative: Some(ast::BlockStatement {
                    statements: vec![Expression::Identifier("y".into()).into()]
                }),
            })
            .into()]
        );
    }

    #[test]
    fn test_function_literal_parsing() {
        let input = "fn(x, y) { x + y; }".to_owned();

        let program = Parser::new(Lexer::new(input.to_owned()))
            .parse_program()
            .expect("Parse errors found");

        assert_eq!(
            program.statements,
            vec![Expression::Function(ast::FunctionLiteral {
                parameters: vec!["x".into(), "y".into()],
                body: ast::BlockStatement {
                    statements: vec![Expression::Infix(ast::InfixExpression {
                        left: Box::new(Expression::Identifier("x".into())),
                        operator: Operator::Plus,
                        right: Box::new(Expression::Identifier("y".into())),
                    })
                    .into()]
                }
            })
            .into()]
        );
    }

    #[test]
    fn test_function_parameter_parsing() {
        let cases = vec![
            ("fn() {};", vec![]),
            ("fn(x) {};", vec!["x".into()]),
            ("fn(x, y, z) {}", vec!["x".into(), "y".into(), "z".into()]),
        ];

        for (input, parameters) in cases {
            let program = Parser::new(Lexer::new(input.to_owned()))
                .parse_program()
                .expect("Parse errors found");

            assert_eq!(
                program.statements,
                vec![Expression::Function(ast::FunctionLiteral {
                    parameters,
                    body: ast::BlockStatement { statements: vec![] }
                })
                .into()]
            );
        }
    }

    #[test]
    fn test_call_expression_parsing() {
        let input = "add(1, 2 * 3, 4 + 5);".to_owned();

        let program = Parser::new(Lexer::new(input))
            .parse_program()
            .expect("Parse errors found");

        assert_eq!(
            program.statements,
            vec![Expression::Call(ast::CallExpression {
                function: Box::new(Expression::Identifier("add".into())),
                arguments: vec![
                    Expression::IntegerLiteral(1),
                    Expression::Infix(ast::InfixExpression {
                        left: Box::new(Expression::IntegerLiteral(2)),
                        operator: Operator::Asterisk,
                        right: Box::new(Expression::IntegerLiteral(3))
                    }),
                    Expression::Infix(ast::InfixExpression {
                        left: Box::new(Expression::IntegerLiteral(4)),
                        operator: Operator::Plus,
                        right: Box::new(Expression::IntegerLiteral(5))
                    })
                ]
            })
            .into()]
        );
    }

    #[test]
    fn test_string_literal_expression() {
        let input = "\"hello world\"".to_owned();

        let program = Parser::new(Lexer::new(input))
            .parse_program()
            .expect("Parse errors found");

        assert_eq!(
            program.statements,
            vec![Expression::String("hello world".to_owned()).into()]
        );
    }

    #[test]
    fn test_parsing_array_literals() {
        let input = "[1, 2 * 2, 3 + 3]".to_owned();

        let program = Parser::new(Lexer::new(input))
            .parse_program()
            .expect("Parse errors found");

        assert_eq!(
            program.statements,
            vec![Expression::Array(
                vec![
                    Expression::IntegerLiteral(1),
                    Expression::Infix(ast::InfixExpression {
                        left: Box::new(Expression::IntegerLiteral(2)),
                        operator: Operator::Asterisk,
                        right: Box::new(Expression::IntegerLiteral(2))
                    }),
                    Expression::Infix(ast::InfixExpression {
                        left: Box::new(Expression::IntegerLiteral(3)),
                        operator: Operator::Plus,
                        right: Box::new(Expression::IntegerLiteral(3))
                    })
                ]
                .into()
            )
            .into()]
        );
    }

    #[test]
    fn test_parsing_empty_array_literals() {
        let input = "[]".to_owned();

        let program = Parser::new(Lexer::new(input))
            .parse_program()
            .expect("Parse errors found");

        assert_eq!(
            program.statements,
            vec![Expression::Array(vec![].into()).into()]
        );
    }

    #[test]
    fn test_parsing_index_expressions() {
        let input = "myArray[1 + 1]".to_owned();

        let program = Parser::new(Lexer::new(input))
            .parse_program()
            .expect("Parse errors found");

        assert_eq!(program.statements.len(), 1);

        assert_eq!(
            program.statements[0],
            Expression::Index(ast::IndexExpression {
                left: Box::new(Expression::Identifier("myArray".into())),
                index: Box::new(Expression::Infix(ast::InfixExpression {
                    left: Box::new(Expression::IntegerLiteral(1)),
                    operator: Operator::Plus,
                    right: Box::new(Expression::IntegerLiteral(1))
                })),
            })
            .into()
        );
    }

    #[test]
    fn test_parsing_hash_literals_string_keys() {
        let input = "{\"one\": 1, \"two\": 2, \"three\": 3}".to_owned();

        let program = Parser::new(Lexer::new(input))
            .parse_program()
            .expect("Parse errors found");

        assert_eq!(
            program.statements,
            vec![Expression::Hash(
                vec![
                    (
                        Expression::String("one".to_owned()),
                        Expression::IntegerLiteral(1)
                    ),
                    (
                        Expression::String("two".to_owned()),
                        Expression::IntegerLiteral(2)
                    ),
                    (
                        Expression::String("three".to_owned()),
                        Expression::IntegerLiteral(3)
                    )
                ]
                .into()
            )
            .into()]
        );
    }

    #[test]
    fn test_parsing_empty_hash_literal() {
        let input = "{}".to_owned();

        let program = Parser::new(Lexer::new(input))
            .parse_program()
            .expect("Parse errors found");

        assert_eq!(
            program.statements,
            vec![Expression::Hash(vec![].into()).into()]
        );
    }
}
