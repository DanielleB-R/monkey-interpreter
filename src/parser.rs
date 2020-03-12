use crate::ast::{Expression, Node, Statement};
use crate::token::TokenType;
use crate::{ast, lexer, token};
use std::collections::HashMap;
use std::rc::Rc;

type PrefixParseFn = fn(&mut Parser) -> ast::Expression;
type InfixParseFn = fn(&mut Parser, ast::Expression) -> ast::Expression;

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

        Self {
            lexer,
            cur_token,
            peek_token,
            errors: Default::default(),
            prefix_parse_fns,
            infix_parse_fns: Default::default(),
        }
    }

    fn register_prefix(&mut self, token_type: TokenType, func: PrefixParseFn) {
        self.prefix_parse_fns.insert(token_type, func);
    }

    fn register_infix(&mut self, token_type: TokenType, func: InfixParseFn) {
        self.infix_parse_fns.insert(token_type, func);
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

        Some(ast::LetStatement { token, name })
    }

    fn parse_return_statement(&mut self) -> Option<ast::ReturnStatement> {
        let token = self.cur_token.clone();

        self.next_token();

        // TODO: We're skipping the expression for now
        while !self.cur_token.is(TokenType::Semicolon) {
            self.next_token();
        }

        Some(ast::ReturnStatement { token })
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
        let prefix = self.prefix_parse_fns.get(&self.cur_token.token_type)?;

        let left_exp = prefix(self);

        Some(left_exp)
    }

    fn parse_identifier(&mut self) -> ast::Expression {
        Expression::Identifier(self.cur_token.clone().into())
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
}
