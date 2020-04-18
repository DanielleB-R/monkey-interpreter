use crate::ast::{Expression, Operator, Statement};
use crate::token::{Token, TokenType};
use crate::{ast, lexer, token};
use std::collections::HashMap;

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
    lexer: std::iter::Peekable<lexer::Lexer>,
    errors: Vec<String>,

    cur_token: Option<token::Token>,

    prefix_parse_fns: HashMap<TokenType, PrefixParseFn>,
    infix_parse_fns: HashMap<TokenType, InfixParseFn>,
}

impl Parser {
    pub fn new(mut lexer: lexer::Lexer) -> Self {
        let cur_token = Some(lexer.next().unwrap());

        let mut prefix_parse_fns: HashMap<TokenType, PrefixParseFn> = Default::default();
        prefix_parse_fns.insert(TokenType::Ident, Self::parse_identifier);
        prefix_parse_fns.insert(TokenType::Int, Self::parse_integer_literal);
        prefix_parse_fns.insert(TokenType::Bang, Self::parse_prefix_expression);
        prefix_parse_fns.insert(TokenType::Minus, Self::parse_prefix_expression);
        prefix_parse_fns.insert(TokenType::True, Self::parse_boolean);
        prefix_parse_fns.insert(TokenType::False, Self::parse_boolean);
        prefix_parse_fns.insert(TokenType::LParen, Self::parse_grouped_expression);
        prefix_parse_fns.insert(TokenType::If, Self::parse_if_expression);
        prefix_parse_fns.insert(TokenType::Function, Self::parse_function_literal);
        prefix_parse_fns.insert(TokenType::String, Self::parse_string_literal);
        prefix_parse_fns.insert(TokenType::LBracket, Self::parse_array_literal);
        prefix_parse_fns.insert(TokenType::LBrace, Self::parse_hash_literal);

        let mut infix_parse_fns: HashMap<TokenType, InfixParseFn> = Default::default();
        infix_parse_fns.insert(TokenType::Plus, Self::parse_infix_expression);
        infix_parse_fns.insert(TokenType::Minus, Self::parse_infix_expression);
        infix_parse_fns.insert(TokenType::Slash, Self::parse_infix_expression);
        infix_parse_fns.insert(TokenType::Asterisk, Self::parse_infix_expression);
        infix_parse_fns.insert(TokenType::Eq, Self::parse_infix_expression);
        infix_parse_fns.insert(TokenType::NotEq, Self::parse_infix_expression);
        infix_parse_fns.insert(TokenType::LT, Self::parse_infix_expression);
        infix_parse_fns.insert(TokenType::GT, Self::parse_infix_expression);
        infix_parse_fns.insert(TokenType::LParen, Self::parse_call_expression);
        infix_parse_fns.insert(TokenType::LBracket, Self::parse_index_expression);

        Self {
            lexer: lexer.peekable(),
            cur_token,
            errors: Default::default(),
            prefix_parse_fns,
            infix_parse_fns,
        }
    }

    fn next_token(&mut self) {
        self.cur_token = Some(self.lexer.next().unwrap());
    }

    fn peek_token(&mut self) -> &token::Token {
        self.lexer.peek().unwrap()
    }

    fn cur_token_is(&self, token_type: TokenType) -> bool {
        self.cur_token.as_ref().unwrap().is(token_type)
    }

    fn cur_token_type(&self) -> TokenType {
        self.cur_token.as_ref().unwrap().into()
    }

    fn take_token(&mut self) -> token::Token {
        self.cur_token.take().unwrap()
    }

    fn skip(&mut self, token_type: TokenType) {
        if self.peek_token().is(token_type) {
            self.next_token();
        }
    }

    pub fn parse_program(mut self) -> Result<ast::Program, Vec<String>> {
        let mut program = ast::Program::default();

        while !self.cur_token_is(TokenType::Eof) {
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
        match self.cur_token_type() {
            TokenType::Let => self.parse_let_statement().map(Statement::Let),
            TokenType::Return => self.parse_return_statement().map(Statement::Return),
            _ => self.parse_expression_statement().map(Statement::Expr),
        }
    }

    fn parse_let_statement(&mut self) -> Option<ast::LetStatement> {
        let token = self.take_token();

        if !self.expect_peek(TokenType::Ident) {
            return None;
        }

        let name = self.take_token().into();

        if !self.expect_peek(TokenType::Assign) {
            return None;
        }

        self.next_token();

        let value = self.parse_expression(Precedence::Lowest)?;

        self.skip(TokenType::Semicolon);

        Some(ast::LetStatement { token, name, value })
    }

    fn parse_return_statement(&mut self) -> Option<ast::ReturnStatement> {
        let token = self.take_token();

        self.next_token();

        let return_value = self.parse_expression(Precedence::Lowest)?;

        self.skip(TokenType::Semicolon);

        Some(ast::ReturnStatement {
            token,
            return_value,
        })
    }

    fn parse_expression_statement(&mut self) -> Option<ast::ExpressionStatement> {
        let expression = self.parse_expression(Precedence::Lowest);

        self.skip(TokenType::Semicolon);

        Some(ast::ExpressionStatement {
            expression: expression?,
        })
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<ast::Expression> {
        let mut left = match self.prefix_parse_fns.get(&self.cur_token_type()) {
            Some(prefix) => prefix(self)?,
            None => {
                self.no_prefix_parse_fn_error(self.cur_token_type());
                return None;
            }
        };

        while !self.peek_token().is(TokenType::Semicolon) && precedence < self.peek_token().into() {
            let token_type: TokenType = self.peek_token().into();
            let infix = *match self.infix_parse_fns.get(&token_type) {
                None => return Some(left),
                Some(infix) => infix,
            };
            self.next_token();
            left = infix(self, left)?
        }

        Some(left)
    }

    fn parse_identifier(&mut self) -> Option<ast::Expression> {
        Some(Expression::Identifier(self.take_token().into()))
    }

    fn parse_integer_literal(&mut self) -> Option<ast::Expression> {
        let token = self.take_token();

        let value: i64 = match token.literal().parse::<i64>() {
            Ok(v) => v,
            Err(_) => {
                self.errors
                    .push(format!("could not parse {} as integer", token.literal()));
                return None;
            }
        };

        Some(Expression::IntegerLiteral(ast::IntegerLiteral {
            token,
            value,
        }))
    }

    fn parse_boolean(&mut self) -> Option<ast::Expression> {
        Some(Expression::Boolean(self.take_token().into()))
    }

    fn expect_peek(&mut self, expected: TokenType) -> bool {
        if self.peek_token().is(expected) {
            self.next_token();
            true
        } else {
            self.peek_error(expected);
            false
        }
    }

    fn peek_error(&mut self, expected: TokenType) {
        let token_type: TokenType = self.peek_token().into();
        self.errors.push(format!(
            "expected next token to be {:?}, got {:?} instead",
            expected, token_type
        ));
    }

    fn no_prefix_parse_fn_error(&mut self, token_type: TokenType) {
        self.errors.push(format!(
            "no prefix parse function for {:?} found",
            token_type
        ))
    }

    fn parse_prefix_expression(&mut self) -> Option<ast::Expression> {
        let token = self.take_token();
        let operator = Operator::from(&token);

        self.next_token();

        let right = Box::new(self.parse_expression(Precedence::Prefix)?);

        Some(Expression::Prefix(ast::PrefixExpression {
            token,
            operator,
            right,
        }))
    }

    fn parse_infix_expression(&mut self, left: ast::Expression) -> Option<ast::Expression> {
        let token = self.take_token();
        let operator = Operator::from(&token);

        let precedence = (&token).into();
        self.next_token();
        let right = Box::new(self.parse_expression(precedence)?);

        Some(Expression::Infix(ast::InfixExpression {
            token,
            left: Box::new(left),
            operator,
            right,
        }))
    }

    fn parse_grouped_expression(&mut self) -> Option<ast::Expression> {
        self.next_token();

        let exp = self.parse_expression(Precedence::Lowest)?;

        if self.expect_peek(TokenType::RParen) {
            Some(exp)
        } else {
            None
        }
    }

    fn parse_if_expression(&mut self) -> Option<ast::Expression> {
        let token = self.take_token();

        if !self.expect_peek(TokenType::LParen) {
            return None;
        }

        self.next_token();
        let condition = Box::new(self.parse_expression(Precedence::Lowest)?);

        if !self.expect_peek(TokenType::RParen) {
            return None;
        }

        if !self.expect_peek(TokenType::LBrace) {
            return None;
        }

        let consequence = self.parse_block_statement();

        let alternative = if self.peek_token().is(TokenType::Else) {
            self.next_token();

            if !self.expect_peek(TokenType::LBrace) {
                return None;
            }

            Some(self.parse_block_statement())
        } else {
            None
        };

        Some(Expression::If(ast::IfExpression {
            token,
            condition,
            consequence,
            alternative,
        }))
    }

    fn parse_block_statement(&mut self) -> ast::BlockStatement {
        let token = self.take_token();
        let mut statements = vec![];

        self.next_token();

        while !self.cur_token_is(TokenType::RBrace) && !self.cur_token_is(TokenType::Eof) {
            if let Some(stmt) = self.parse_statement() {
                statements.push(stmt);
            }
            self.next_token();
        }

        ast::BlockStatement { token, statements }
    }

    fn parse_function_literal(&mut self) -> Option<ast::Expression> {
        let token = self.take_token();

        if !self.expect_peek(TokenType::LParen) {
            return None;
        }

        let parameters = self.parse_function_parameters()?;

        if !self.expect_peek(TokenType::LBrace) {
            return None;
        }

        let body = self.parse_block_statement();
        Some(Expression::Function(ast::FunctionLiteral {
            token,
            parameters,
            body,
        }))
    }

    fn parse_function_parameters(&mut self) -> Option<Vec<ast::Identifier>> {
        let mut identifiers = Default::default();

        if self.peek_token().is(TokenType::RParen) {
            self.next_token();
            return Some(identifiers);
        }

        self.next_token();

        identifiers.push(self.take_token().into());

        while self.peek_token().is(TokenType::Comma) {
            self.next_token();
            self.next_token();
            identifiers.push(self.take_token().into());
        }

        if self.expect_peek(TokenType::RParen) {
            Some(identifiers)
        } else {
            None
        }
    }

    fn parse_call_expression(&mut self, function: ast::Expression) -> Option<ast::Expression> {
        let token = self.take_token();
        let arguments = self.parse_expression_list(TokenType::RParen)?;

        Some(Expression::Call(ast::CallExpression {
            token,
            function: Box::new(function),
            arguments,
        }))
    }

    fn parse_expression_list(&mut self, end: TokenType) -> Option<Vec<ast::Expression>> {
        let mut list = Default::default();

        if self.peek_token().is(end) {
            self.next_token();
            return Some(list);
        }

        self.next_token();

        list.push(self.parse_expression(Precedence::Lowest)?);

        while self.peek_token().is(TokenType::Comma) {
            self.next_token();
            self.next_token();
            list.push(self.parse_expression(Precedence::Lowest)?);
        }

        if self.expect_peek(end) {
            Some(list)
        } else {
            None
        }
    }

    fn parse_string_literal(&mut self) -> Option<ast::Expression> {
        Some(ast::Expression::String(self.take_token().into()))
    }

    fn parse_array_literal(&mut self) -> Option<ast::Expression> {
        let token = self.take_token();
        let elements = self.parse_expression_list(TokenType::RBracket)?;
        Some(Expression::Array(ast::ArrayLiteral { token, elements }))
    }

    fn parse_index_expression(&mut self, left: ast::Expression) -> Option<ast::Expression> {
        let token = self.take_token();

        self.next_token();
        let index = self.parse_expression(Precedence::Lowest)?;

        if self.expect_peek(TokenType::RBracket) {
            Some(Expression::Index(ast::IndexExpression {
                token,
                left: Box::new(left),
                index: Box::new(index),
            }))
        } else {
            None
        }
    }

    fn parse_hash_literal(&mut self) -> Option<ast::Expression> {
        let token = self.take_token();
        let mut pairs = vec![];

        while !self.peek_token().is(TokenType::RBrace) {
            self.next_token();

            let key = self.parse_expression(Precedence::Lowest)?;

            if !self.expect_peek(TokenType::Colon) {
                return None;
            }

            self.next_token();
            let value = self.parse_expression(Precedence::Lowest)?;
            pairs.push((key, value));

            if !self.peek_token().is(TokenType::RBrace) && !self.expect_peek(TokenType::Comma) {
                return None;
            }
        }

        if self.expect_peek(TokenType::RBrace) {
            Some(Expression::Hash(ast::HashLiteral { token, pairs }))
        } else {
            None
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::lexer::Lexer;
    use crate::token::Token;

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

        let cases = [
            ("x", Expected::Int(5)),
            ("y", Expected::Bool(true)),
            ("foobar", Expected::Ident("y")),
        ];

        for (case, stmt) in cases.iter().zip(program.statements.iter()) {
            test_let_statement(stmt, case.0);
            case.1.test(&stmt.pull_let().value)
        }
    }

    fn test_let_statement(stmt: &Statement, name: &str) {
        let let_stmt = stmt.pull_let();

        assert_eq!(let_stmt.token, Token::Let);
        assert_eq!(let_stmt.name.value, name);
        assert_eq!(let_stmt.name.token.literal(), name);
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

        assert_eq!(program.statements.len(), 3);

        let expected = [
            Expected::Int(5),
            Expected::Bool(true),
            Expected::Ident("foobar"),
        ];

        for (stmt, value) in program.statements.iter().zip(expected.iter()) {
            let ret_stmt = stmt.pull_return();
            assert_eq!(ret_stmt.token, Token::Return);
            value.test(&ret_stmt.return_value);
        }
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;".to_owned();

        let program = Parser::new(Lexer::new(input))
            .parse_program()
            .expect("Parse errors found");

        assert_eq!(program.statements.len(), 1);

        let stmt = program.statements[0].pull_expr();
        test_identifier(&stmt.expression, "foobar");
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = "5;".to_owned();

        let program = Parser::new(Lexer::new(input))
            .parse_program()
            .expect("Parse errors found");

        assert_eq!(program.statements.len(), 1);

        let stmt = program.statements[0].pull_expr();
        test_integer_literal(&stmt.expression, 5);
    }

    #[test]
    fn test_parsing_prefix_expressions() {
        let cases = [
            ("!5;", Operator::Bang, Expected::Int(5)),
            ("-15;", Operator::Minus, Expected::Int(15)),
            ("!foobar;", Operator::Bang, Expected::Ident("foobar")),
            ("-foobar;", Operator::Minus, Expected::Ident("foobar")),
            ("!true;", Operator::Bang, Expected::Bool(true)),
            ("!false;", Operator::Bang, Expected::Bool(false)),
        ];

        for (input, operator, value) in cases.iter() {
            let program = Parser::new(Lexer::new((*input).to_owned()))
                .parse_program()
                .expect("Parse errors found");

            assert_eq!(program.statements.len(), 1);

            let exp = program.statements[0].pull_expr().expression.pull_prefix();
            assert_eq!(exp.operator, *operator);
            value.test(&exp.right);
        }
    }

    fn test_integer_literal(expr: &ast::Expression, value: i64) {
        let literal = expr.pull_integer();
        assert_eq!(literal.value, value);
        assert_eq!(literal.token.literal(), value.to_string());
    }

    #[test]
    fn test_parsing_infix_expressions() {
        let cases = [
            ("5 + 5;", Expected::Int(5), Operator::Plus, Expected::Int(5)),
            (
                "5 - 5;",
                Expected::Int(5),
                Operator::Minus,
                Expected::Int(5),
            ),
            (
                "5 * 5;",
                Expected::Int(5),
                Operator::Asterisk,
                Expected::Int(5),
            ),
            (
                "5 / 5;",
                Expected::Int(5),
                Operator::Slash,
                Expected::Int(5),
            ),
            ("5 > 5;", Expected::Int(5), Operator::GT, Expected::Int(5)),
            ("5 < 5;", Expected::Int(5), Operator::LT, Expected::Int(5)),
            ("5 == 5;", Expected::Int(5), Operator::Eq, Expected::Int(5)),
            (
                "5 != 5;",
                Expected::Int(5),
                Operator::NotEq,
                Expected::Int(5),
            ),
            (
                "foobar + barfoo;",
                Expected::Ident("foobar"),
                Operator::Plus,
                Expected::Ident("barfoo"),
            ),
            (
                "foobar - barfoo;",
                Expected::Ident("foobar"),
                Operator::Minus,
                Expected::Ident("barfoo"),
            ),
            (
                "foobar * barfoo;",
                Expected::Ident("foobar"),
                Operator::Asterisk,
                Expected::Ident("barfoo"),
            ),
            (
                "foobar / barfoo;",
                Expected::Ident("foobar"),
                Operator::Slash,
                Expected::Ident("barfoo"),
            ),
            (
                "foobar > barfoo;",
                Expected::Ident("foobar"),
                Operator::GT,
                Expected::Ident("barfoo"),
            ),
            (
                "foobar < barfoo;",
                Expected::Ident("foobar"),
                Operator::LT,
                Expected::Ident("barfoo"),
            ),
            (
                "foobar == barfoo;",
                Expected::Ident("foobar"),
                Operator::Eq,
                Expected::Ident("barfoo"),
            ),
            (
                "foobar != barfoo;",
                Expected::Ident("foobar"),
                Operator::NotEq,
                Expected::Ident("barfoo"),
            ),
            (
                "true == true",
                Expected::Bool(true),
                Operator::Eq,
                Expected::Bool(true),
            ),
            (
                "true != false",
                Expected::Bool(true),
                Operator::NotEq,
                Expected::Bool(false),
            ),
            (
                "false == false",
                Expected::Bool(false),
                Operator::Eq,
                Expected::Bool(false),
            ),
        ];

        for (input, left, op, right) in cases.iter() {
            let program = Parser::new(Lexer::new((*input).to_owned()))
                .parse_program()
                .expect("Parse errors found");

            assert_eq!(program.statements.len(), 1);

            let stmt = program.statements[0].pull_expr();
            test_infix_expression(&stmt.expression, left, *op, right);
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

        for (input, output) in cases.iter() {
            let program = Parser::new(Lexer::new((*input).to_owned()))
                .parse_program()
                .expect("Parse errors found");

            assert_eq!(format!("{}", program), *output);
        }
    }

    #[test]
    fn test_boolean_expression() {
        let cases = [
            ("true;", Expected::Bool(true)),
            ("false;", Expected::Bool(false)),
        ];

        for (input, expected) in cases.iter() {
            let program = Parser::new(Lexer::new((*input).to_owned()))
                .parse_program()
                .expect("Parse errors found");

            assert_eq!(program.statements.len(), 1);

            let stmt = program.statements[0].pull_expr();
            expected.test(&stmt.expression);
        }
    }

    fn test_identifier(exp: &ast::Expression, value: &str) {
        let ident = exp.pull_identifier();
        assert_eq!(ident.value, value);
        assert_eq!(ident.token.literal(), value);
    }

    enum Expected<'a> {
        Int(i64),
        Ident(&'a str),
        Bool(bool),
    }

    impl<'a> Expected<'a> {
        fn test(&self, exp: &ast::Expression) {
            match self {
                Self::Int(n) => test_integer_literal(exp, *n),
                Self::Ident(s) => test_identifier(exp, s),
                Self::Bool(b) => test_boolean_literal(exp, *b),
            }
        }
    }

    fn test_boolean_literal(exp: &ast::Expression, value: bool) {
        let b = exp.pull_boolean();
        assert_eq!(b.value, value);
    }

    fn test_infix_expression(
        exp: &ast::Expression,
        left: &Expected,
        operator: Operator,
        right: &Expected,
    ) {
        let infix_exp = exp.pull_infix();

        left.test(&infix_exp.left);
        assert_eq!(infix_exp.operator, operator);
        right.test(&infix_exp.right);
    }

    #[test]
    fn test_if_expression() {
        let input = "if (x > y) { x }".to_owned();

        let program = Parser::new(Lexer::new(input.to_owned()))
            .parse_program()
            .expect("Parse errors found");

        assert_eq!(program.statements.len(), 1);
        let expr = program.statements[0].pull_expr().expression.pull_if();
        test_infix_expression(
            &expr.condition,
            &Expected::Ident("x"),
            Operator::GT,
            &Expected::Ident("y"),
        );

        assert_eq!(expr.consequence.statements.len(), 1);
        let stmt = expr.consequence.statements[0].pull_expr();
        test_identifier(&stmt.expression, "x");
        assert!(expr.alternative.is_none());
    }

    #[test]
    fn test_if_else_expression() {
        let input = "if (x > y) { x } else { y }".to_owned();

        let program = Parser::new(Lexer::new(input.to_owned()))
            .parse_program()
            .expect("Parse errors found");

        assert_eq!(program.statements.len(), 1);
        let expr = program.statements[0].pull_expr().expression.pull_if();
        test_infix_expression(
            &expr.condition,
            &Expected::Ident("x"),
            Operator::GT,
            &Expected::Ident("y"),
        );

        assert_eq!(expr.consequence.statements.len(), 1);
        let stmt = expr.consequence.statements[0].pull_expr();
        test_identifier(&stmt.expression, "x");

        match &expr.alternative {
            Some(ast::BlockStatement { statements, .. }) => {
                assert_eq!(statements.len(), 1);
                let stmt = statements[0].pull_expr();
                test_identifier(&stmt.expression, "y");
            }
            _ => panic!(),
        }
    }

    #[test]
    fn test_function_literal_parsing() {
        let input = "fn(x, y) { x + y; }".to_owned();

        let program = Parser::new(Lexer::new(input.to_owned()))
            .parse_program()
            .expect("Parse errors found");

        assert_eq!(program.statements.len(), 1);

        let expr = program.statements[0].pull_expr().expression.pull_function();

        assert_eq!(expr.parameters.len(), 2);
        assert_eq!(expr.parameters[0].value, "x");
        assert_eq!(expr.parameters[1].value, "y");

        assert_eq!(expr.body.statements.len(), 1);

        let body_stmt = expr.body.statements[0].pull_expr();
        test_infix_expression(
            &body_stmt.expression,
            &Expected::Ident("x"),
            Operator::Plus,
            &Expected::Ident("y"),
        );
    }

    #[test]
    fn test_function_parameter_parsing() {
        let cases = [
            ("fn() {};", vec![]),
            ("fn(x) {};", vec!["x"]),
            ("fn(x, y, z) {}", vec!["x", "y", "z"]),
        ];

        for (input, params) in cases.iter() {
            let program = Parser::new(Lexer::new((*input).to_owned()))
                .parse_program()
                .expect("Parse errors found");

            let function = program.statements[0].pull_expr().expression.pull_function();
            assert_eq!(function.parameters.len(), params.len());

            for (actual, expected) in function.parameters.iter().zip(params.iter()) {
                assert_eq!(&actual.value, expected);
            }
        }
    }

    #[test]
    fn test_call_expression_parsing() {
        let input = "add(1, 2 * 3, 4 + 5);".to_owned();

        let program = Parser::new(Lexer::new(input))
            .parse_program()
            .expect("Parse errors found");

        assert_eq!(program.statements.len(), 1);

        let exp = program.statements[0].pull_expr().expression.pull_call();

        test_identifier(&exp.function, "add");

        assert_eq!(exp.arguments.len(), 3);

        test_integer_literal(&exp.arguments[0], 1);
        test_infix_expression(
            &exp.arguments[1],
            &Expected::Int(2),
            Operator::Asterisk,
            &Expected::Int(3),
        );
        test_infix_expression(
            &exp.arguments[2],
            &Expected::Int(4),
            Operator::Plus,
            &Expected::Int(5),
        );
    }

    #[test]
    fn test_string_literal_expression() {
        let input = "\"hello world\"".to_owned();

        let program = Parser::new(Lexer::new(input))
            .parse_program()
            .expect("Parse errors found");

        let literal = program.statements[0].pull_expr().expression.pull_string();
        assert_eq!(literal.value, "hello world");
    }

    #[test]
    fn test_parsing_array_literals() {
        let input = "[1, 2 * 2, 3 + 3]".to_owned();

        let program = Parser::new(Lexer::new(input))
            .parse_program()
            .expect("Parse errors found");

        assert_eq!(program.statements.len(), 1);
        let array = program.statements[0].pull_expr().expression.pull_array();
        assert_eq!(array.elements.len(), 3);

        test_integer_literal(&array.elements[0], 1);
        test_infix_expression(
            &array.elements[1],
            &Expected::Int(2),
            Operator::Asterisk,
            &Expected::Int(2),
        );
        test_infix_expression(
            &array.elements[2],
            &Expected::Int(3),
            Operator::Plus,
            &Expected::Int(3),
        );
    }

    #[test]
    fn test_parsing_empty_array_literals() {
        let input = "[]".to_owned();

        let program = Parser::new(Lexer::new(input))
            .parse_program()
            .expect("Parse errors found");

        assert_eq!(program.statements.len(), 1);
        let array = program.statements[0].pull_expr().expression.pull_array();
        assert_eq!(array.elements.len(), 0);
    }

    #[test]
    fn test_parsing_index_expressions() {
        let input = "myArray[1 + 1]".to_owned();

        let program = Parser::new(Lexer::new(input))
            .parse_program()
            .expect("Parse errors found");

        assert_eq!(program.statements.len(), 1);
        let index = program.statements[0].pull_expr().expression.pull_index();

        test_identifier(&index.left, "myArray");
        test_infix_expression(
            &index.index,
            &Expected::Int(1),
            Operator::Plus,
            &Expected::Int(1),
        );
    }

    #[test]
    fn test_parsing_hash_literals_string_keys() {
        let input = "{\"one\": 1, \"two\": 2, \"three\": 3}".to_owned();

        let program = Parser::new(Lexer::new(input))
            .parse_program()
            .expect("Parse errors found");

        let hash = program.statements[0].pull_expr().expression.pull_hash();

        assert_eq!(hash.pairs.len(), 3);
        assert_eq!(hash.pairs[0].0.pull_string().value, "one");
        assert_eq!(hash.pairs[0].1.pull_integer().value, 1);
        assert_eq!(hash.pairs[1].0.pull_string().value, "two");
        assert_eq!(hash.pairs[1].1.pull_integer().value, 2);
        assert_eq!(hash.pairs[2].0.pull_string().value, "three");
        assert_eq!(hash.pairs[2].1.pull_integer().value, 3);
    }

    #[test]
    fn test_parsing_empty_hash_literal() {
        let input = "{}".to_owned();

        let program = Parser::new(Lexer::new(input))
            .parse_program()
            .expect("Parse errors found");

        let hash = program.statements[0].pull_expr().expression.pull_hash();

        assert_eq!(hash.pairs.len(), 0);
    }
}
