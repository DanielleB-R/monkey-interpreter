use crate::ast::{Expression, Node, Statement};
use crate::token::TokenType;
use crate::{ast, lexer, token};
use std::collections::HashMap;
use std::rc::Rc;

type PrefixParseFn = fn(&mut Parser) -> Option<ast::Expression>;
type InfixParseFn = fn(&mut Parser, ast::Expression) -> Option<ast::Expression>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

impl From<TokenType> for Precedence {
    fn from(t: TokenType) -> Self {
        match t {
            TokenType::Eq => Self::Equals,
            TokenType::NotEq => Self::Equals,
            TokenType::LT => Self::LessGreater,
            TokenType::GT => Self::LessGreater,
            TokenType::Plus => Self::Sum,
            TokenType::Minus => Self::Sum,
            TokenType::Slash => Self::Product,
            TokenType::Asterisk => Self::Product,
            _ => Self::Lowest,
        }
    }
}

pub struct Parser {
    lexer: lexer::Lexer,
    errors: Vec<String>,

    cur_token: token::Token,
    peek_token: token::Token,

    prefix_parse_fns: HashMap<TokenType, PrefixParseFn>,
    infix_parse_fns: HashMap<TokenType, InfixParseFn>,
}

impl Parser {
    pub fn new(mut lexer: lexer::Lexer) -> Self {
        let cur_token = lexer.next_token();
        let peek_token = lexer.next_token();

        let mut prefix_parse_fns: HashMap<TokenType, PrefixParseFn> = Default::default();
        prefix_parse_fns.insert(TokenType::Ident, Self::parse_identifier);
        prefix_parse_fns.insert(TokenType::Int, Self::parse_integer_literal);
        prefix_parse_fns.insert(TokenType::Bang, Self::parse_prefix_expression);
        prefix_parse_fns.insert(TokenType::Minus, Self::parse_prefix_expression);

        let mut infix_parse_fns: HashMap<TokenType, InfixParseFn> = Default::default();
        infix_parse_fns.insert(TokenType::Plus, Self::parse_infix_expression);
        infix_parse_fns.insert(TokenType::Minus, Self::parse_infix_expression);
        infix_parse_fns.insert(TokenType::Slash, Self::parse_infix_expression);
        infix_parse_fns.insert(TokenType::Asterisk, Self::parse_infix_expression);
        infix_parse_fns.insert(TokenType::Eq, Self::parse_infix_expression);
        infix_parse_fns.insert(TokenType::NotEq, Self::parse_infix_expression);
        infix_parse_fns.insert(TokenType::LT, Self::parse_infix_expression);
        infix_parse_fns.insert(TokenType::GT, Self::parse_infix_expression);

        Self {
            lexer,
            cur_token,
            peek_token,
            errors: Default::default(),
            prefix_parse_fns,
            infix_parse_fns,
        }
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    pub fn parse_program(mut self) -> Result<ast::Program, Vec<String>> {
        let mut program = ast::Program::default();

        while !self.cur_token.is(TokenType::Eof) {
            if let Some(stmt) = self.parse_statement() {
                program.statements.push(stmt)
            }
            self.next_token();
        }

        if self.errors.is_empty() {
            Ok(program)
        } else {
            Err(self.errors)
        }
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.cur_token.token_type {
            TokenType::Let => self.parse_let_statement().map(Statement::Let),
            TokenType::Return => self.parse_return_statement().map(Statement::Return),
            _ => self.parse_expression_statement().map(Statement::Expr),
        }
    }

    fn parse_let_statement(&mut self) -> Option<ast::LetStatement> {
        let token = self.cur_token.clone();

        if !self.expect_peek(TokenType::Ident) {
            return None;
        }

        let name: Rc<ast::Identifier> = Rc::new(self.cur_token.clone().into());

        if !self.expect_peek(TokenType::Assign) {
            return None;
        }

        // TODO: We're skipping the expression for now
        while !self.cur_token.is(TokenType::Semicolon) {
            self.next_token();
        }

        Some(ast::LetStatement {
            token,
            name,
            value: Expression::Nil,
        })
    }

    fn parse_return_statement(&mut self) -> Option<ast::ReturnStatement> {
        let token = self.cur_token.clone();

        self.next_token();

        // TODO: We're skipping the expression for now
        while !self.cur_token.is(TokenType::Semicolon) {
            self.next_token();
        }

        Some(ast::ReturnStatement {
            token,
            return_value: Expression::Nil,
        })
    }

    fn parse_expression_statement(&mut self) -> Option<ast::ExpressionStatement> {
        let token = self.cur_token.clone();

        let expression = self.parse_expression(Precedence::Lowest);

        if self.peek_token.is(TokenType::Semicolon) {
            self.next_token();
        }

        Some(ast::ExpressionStatement {
            token,
            expression: expression?,
        })
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<ast::Expression> {
        let mut left = match self.prefix_parse_fns.get(&self.cur_token.token_type) {
            Some(prefix) => prefix(self)?,
            None => {
                self.no_prefix_parse_fn_error(self.cur_token.token_type);
                return None;
            }
        };

        while !self.peek_token.is(TokenType::Semicolon)
            && precedence < self.peek_token.token_type.into()
        {
            let infix = *match self.infix_parse_fns.get(&self.peek_token.token_type) {
                None => return Some(left),
                Some(infix) => infix,
            };
            self.next_token();
            left = infix(self, left)?
        }

        Some(left)
    }

    fn parse_identifier(&mut self) -> Option<ast::Expression> {
        Some(Expression::Identifier(self.cur_token.clone().into()))
    }

    fn parse_integer_literal(&mut self) -> Option<ast::Expression> {
        let token = self.cur_token.clone();

        let value: i64 = match self.cur_token.literal.parse::<i64>() {
            Ok(v) => v,
            Err(_) => {
                self.errors.push(format!(
                    "could not parse {} as integer",
                    self.cur_token.literal
                ));
                return None;
            }
        };

        Some(Expression::IntegerLiteral(ast::IntegerLiteral {
            token,
            value,
        }))
    }

    fn expect_peek(&mut self, expected: TokenType) -> bool {
        if self.peek_token.is(expected) {
            self.next_token();
            true
        } else {
            self.peek_error(expected);
            false
        }
    }

    fn peek_error(&mut self, expected: TokenType) {
        self.errors.push(format!(
            "expected next token to be {:?}, got {:?} instead",
            expected, self.peek_token.token_type
        ));
    }

    fn no_prefix_parse_fn_error(&mut self, token_type: TokenType) {
        self.errors.push(format!(
            "no prefix parse function for {:?} found",
            token_type
        ))
    }

    fn parse_prefix_expression(&mut self) -> Option<ast::Expression> {
        let token = self.cur_token.clone();
        let operator = token.literal.clone();

        self.next_token();

        let right = Box::new(self.parse_expression(Precedence::Prefix)?);

        Some(Expression::Prefix(ast::PrefixExpression {
            token,
            operator,
            right,
        }))
    }

    fn parse_infix_expression(&mut self, left: ast::Expression) -> Option<ast::Expression> {
        let token = self.cur_token.clone();
        let operator = token.literal.clone();

        let precedence = self.cur_token.token_type.into();
        self.next_token();
        let right = Box::new(self.parse_expression(precedence)?);

        Some(Expression::Infix(ast::InfixExpression {
            token,
            left: Box::new(left),
            operator,
            right,
        }))
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_let_statements() {
        let input = "
let x = 5;
let y = 10;
let foobar = 838383;
"
        .to_owned();
        let lexer = lexer::Lexer::new(input);
        let parser = Parser::new(lexer);

        let program = parser.parse_program().expect("Parse errors found");

        assert_eq!(program.statements.len(), 3);

        let cases = [("x",), ("y",), ("foobar",)];

        for (i, case) in cases.iter().enumerate() {
            test_let_statement(&program.statements[i], case.0);
        }
    }

    fn test_let_statement(stmt: &Statement, name: &str) {
        assert_eq!(stmt.token_literal(), "let");

        match stmt {
            Statement::Let(let_stmt) => {
                assert_eq!(let_stmt.name.value, name);
                assert_eq!(let_stmt.name.token_literal(), name);
            }
            _ => panic!(),
        }
    }

    #[test]
    fn test_return_statements() {
        let input = "
return 5;
return 10;
return 993322;
"
        .to_owned();
        let lexer = lexer::Lexer::new(input);
        let parser = Parser::new(lexer);

        let program = parser.parse_program().expect("Parse errors found");

        assert_eq!(program.statements.len(), 3);

        for stmt in program.statements.iter() {
            match stmt {
                Statement::Return(ret_stmt) => {
                    assert_eq!(ret_stmt.token_literal(), "return");
                }
                _ => panic!(),
            }
        }
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;".to_owned();

        let lexer = lexer::Lexer::new(input);
        let parser = Parser::new(lexer);

        let program = parser.parse_program().expect("Parse errors found");

        assert_eq!(program.statements.len(), 1);

        match &program.statements[0] {
            Statement::Expr(stmt) => match &stmt.expression {
                Expression::Identifier(ident) => {
                    assert_eq!(ident.value, "foobar");
                    assert_eq!(ident.token_literal(), "foobar");
                }
                _ => panic!(),
            },
            _ => panic!(),
        }
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = "5;".to_owned();

        let lexer = lexer::Lexer::new(input);
        let parser = Parser::new(lexer);

        let program = parser.parse_program().expect("Parse errors found");

        assert_eq!(program.statements.len(), 1);

        match &program.statements[0] {
            Statement::Expr(stmt) => test_integer_literal(&stmt.expression, 5),
            _ => panic!(),
        }
    }

    #[test]
    fn test_parsing_prefix_expressions() {
        let cases = [("!5;", "!", 5), ("-15;", "-", 15)];

        for (input, operator, value) in cases.iter() {
            let lexer = lexer::Lexer::new((*input).to_owned());
            let parser = Parser::new(lexer);

            let program = parser.parse_program().expect("Parse errors found");

            assert_eq!(program.statements.len(), 1);

            match &program.statements[0] {
                Statement::Expr(stmt) => match &stmt.expression {
                    Expression::Prefix(exp) => {
                        assert_eq!(exp.operator, *operator);
                        test_integer_literal(&exp.right, *value);
                    }
                    _ => panic!(),
                },
                _ => panic!(),
            }
        }
    }

    fn test_integer_literal(expr: &ast::Expression, value: i64) {
        match expr {
            Expression::IntegerLiteral(literal) => {
                assert_eq!(literal.value, value);
                assert_eq!(literal.token_literal(), value.to_string());
            }
            _ => panic!(),
        }
    }

    #[test]
    fn test_parsing_infix_expressions() {
        let cases = [
            ("5 + 5;", 5, "+", 5),
            ("5 - 5;", 5, "-", 5),
            ("5 * 5;", 5, "*", 5),
            ("5 / 5;", 5, "/", 5),
            ("5 > 5;", 5, ">", 5),
            ("5 < 5;", 5, "<", 5),
            ("5 == 5;", 5, "==", 5),
            ("5 != 5;", 5, "!=", 5),
        ];

        for (input, left, op, right) in cases.iter() {
            let lexer = lexer::Lexer::new((*input).to_owned());
            let parser = Parser::new(lexer);

            let program = parser.parse_program().expect("Parse errors found");

            assert_eq!(program.statements.len(), 1);

            match &program.statements[0] {
                Statement::Expr(stmt) => match &stmt.expression {
                    Expression::Infix(exp) => {
                        test_integer_literal(&exp.left, *left);
                        assert_eq!(exp.operator, *op);
                        test_integer_literal(&exp.right, *right);
                    }
                    _ => panic!(),
                },
                _ => panic!(),
            }
        }
    }

    #[test]
    fn test_operator_precedence_parsing() {
        let cases = [
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
        ];

        for (input, output) in cases.iter() {
            let lexer = lexer::Lexer::new((*input).to_owned());
            let parser = Parser::new(lexer);

            let program = parser.parse_program().expect("Parse errors found");

            assert_eq!(format!("{}", program), *output);
        }
    }
}
