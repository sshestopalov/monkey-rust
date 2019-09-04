use std::collections::HashMap;
use std::{error::Error, fmt};

use crate::ast::array::ArrayLiteral;
use crate::ast::boolean::Boolean;
use crate::ast::call::CallExpression;
use crate::ast::expression::Expression;
use crate::ast::function::FunctionLiteral;
use crate::ast::identifier::Identifier;
use crate::ast::if_expression::If;
use crate::ast::index_expression::IndexExpression;
use crate::ast::infix::Infix;
use crate::ast::int::IntegerLiteral;
use crate::ast::prefix::Prefix;
use crate::ast::program::Program;
use crate::ast::statement::{
    BlockStatement, ExpressionStatement, LetStatement, ReturnStatement, Statement,
};
use crate::ast::string::StringLiteral;
use crate::lexer::Lexer;
use crate::token::{Token, TokenType};

#[derive(Copy, Clone, PartialEq, PartialOrd)]
enum Precedence {
    Lowest = 1,
    Equals = 2,      // ==
    LessGreater = 3, // > or <
    Sum = 4,         // +
    Product = 5,     // *
    Prefix = 6,      //-X or !X
    Call = 7,        // function(X)
    Index = 8,
}

#[derive(Debug)]
pub struct ParseError;
impl Error for ParseError {}
impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Failed to parse.")
    }
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    current_token: Option<Token>,
    peek_token: Option<Token>,
    pub errors: Vec<String>,
    precedences: HashMap<TokenType, Precedence>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Parser {
        let mut parser = Parser {
            lexer: lexer,
            current_token: None,
            peek_token: None,
            errors: vec![],
            precedences: HashMap::with_capacity(8),
        };
        let precedences = vec![
            (TokenType::Equal, Precedence::Equals),
            (TokenType::NotEqual, Precedence::Equals),
            (TokenType::Lt, Precedence::LessGreater),
            (TokenType::Gt, Precedence::LessGreater),
            (TokenType::Plus, Precedence::Sum),
            (TokenType::Minus, Precedence::Sum),
            (TokenType::Slash, Precedence::Product),
            (TokenType::Asterisk, Precedence::Product),
            (TokenType::LParen, Precedence::Call),
            (TokenType::LBracket, Precedence::Index),
        ];

        for p in precedences {
            parser.precedences.insert(p.0, p.1);
        }

        parser.next_token();
        parser.next_token();

        parser
    }

    pub fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next();
    }

    pub fn parse_program(&mut self) -> Result<Program, ParseError> {
        let mut program = Program::new();

        while let Some(token) = &self.current_token {
            if token.token_type == TokenType::Semicolon {
                self.next_token();
            }
            let statement = self.parse_statement();
            match statement {
                Some(statement) => program.statements.push(statement),
                None => self.next_token(),
            }
        }
        Ok(program)
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        if let Some(token) = &self.current_token {
            return match token.token_type {
                TokenType::Let => self.parse_let_statement(),
                TokenType::Return => self.parse_return_statement(),
                _ => self.parse_expression_statement(),
            };
        }
        None
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        if let Some(token) = &self.current_token {
            let mut stmt = LetStatement::new(&token);

            if !self.expect_peek(TokenType::Identifier) {
                return None;
            }

            let identifier = self.current_token.clone().unwrap();

            stmt.name = Some(Identifier::new(&identifier, &identifier.literal));

            if !self.expect_peek(TokenType::Assign) {
                return None;
            }

            self.next_token();

            while self.current_token.is_some() && !self.current_token_is(TokenType::Semicolon) {
                stmt.value = self.parse_expression(Precedence::Lowest);

                self.next_token();
            }
            self.next_token();
            return Some(Statement::Let(stmt));
        }
        None
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        if let Some(token) = &self.current_token {
            let mut stmt = ReturnStatement::new(&token);

            self.next_token();

            stmt.return_value = self.parse_expression(Precedence::Lowest);

            if self.peek_token_is(TokenType::Semicolon) {
                self.next_token();
            }

            return Some(Statement::Return(stmt));
        }
        None
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        if let Some(token) = &self.current_token {
            let mut stmt = ExpressionStatement::new(token);

            stmt.expression = self.parse_expression(Precedence::Lowest);

            if self.peek_token_is(TokenType::Semicolon) {
                self.next_token();
            }

            if self.peek_token.is_none() {
                self.next_token();
            }

            return Some(Statement::Expression(stmt));
        }
        None
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        let mut left_expr;
        if self.is_prefix_expression() {
            left_expr = self.parse_prefix();
        } else {
            left_expr = self.parse_identifier();
        }

        while !self.peek_token_is(TokenType::Semicolon) && precedence < self.peek_precedence() {
            if !self.is_infix_expression() {
                return left_expr;
            }

            self.next_token();

            left_expr = self.parse_infix(left_expr);
        }
        left_expr
    }

    fn parse_prefix(&mut self) -> Option<Expression> {
        if let Some(token) = &self.current_token {
            return match token.token_type {
                TokenType::LParen => self.parse_grouped_expression(),
                TokenType::LBracket => self.parse_array_expression(),
                TokenType::If => self.parse_if_expression(),
                TokenType::Function => self.parse_function_literal(),
                _ => {
                    let mut prefix = Prefix::new(token, &token.literal, None);

                    self.next_token();

                    prefix.right = Box::new(self.parse_expression(Precedence::Prefix));
                    Some(Expression::Prefix(prefix))
                }
            };
        }
        None
    }

    fn parse_infix(&mut self, left: Option<Expression>) -> Option<Expression> {
        if let Some(token) = &self.current_token {
            if token.token_type == TokenType::LParen {
                if let Some(l) = left {
                    return self.parse_call_expression(l);
                }
            }

            if token.token_type == TokenType::LBracket {
                if let Some(l) = left {
                    return self.parse_index_expression(l);
                }
            }
            let mut expression = Infix::new(token, left, &token.literal, None);
            let precedence = self.current_precedence();

            self.next_token();

            expression.right = Box::new(self.parse_expression(precedence));

            return Some(Expression::Infix(expression));
        }
        None
    }

    fn parse_identifier(&mut self) -> Option<Expression> {
        if let Some(token) = &self.current_token {
            return match token.token_type {
                TokenType::Identifier => Some(Expression::Identifier(Identifier::new(
                    token,
                    &token.literal,
                ))),
                TokenType::Int => Some(Expression::Int(IntegerLiteral::new(token, &token.literal))),
                TokenType::Str => Some(Expression::Str(StringLiteral::new(token, &token.literal))),
                TokenType::True => Some(Expression::Bool(Boolean::new(token, true))),
                TokenType::False => Some(Expression::Bool(Boolean::new(token, false))),
                _ => None,
            };
        }
        None
    }

    fn parse_grouped_expression(&mut self) -> Option<Expression> {
        self.next_token();

        let expression = self.parse_expression(Precedence::Lowest);
        if !self.expect_peek(TokenType::RParen) {
            return None;
        }

        expression
    }

    fn parse_array_expression(&mut self) -> Option<Expression> {
        if let Some(token) = &self.current_token {
            let mut array = ArrayLiteral::new(token);

            array.elements = self
                .parse_expression_list(TokenType::RBracket)
                .unwrap_or(vec![]);

            return Some(Expression::Array(array));
        }

        None
    }

    fn parse_index_expression(&mut self, left: Expression) -> Option<Expression> {
        let token = self.current_token.clone().unwrap();

        self.next_token();

        let index = self.parse_expression(Precedence::Lowest);

        if !self.expect_peek(TokenType::RBracket) {
            return None;
        }

        return Some(Expression::Index(IndexExpression::new(
            &token,
            left,
            index.unwrap(),
        )));
    }
    fn parse_expression_list(&mut self, end: TokenType) -> Option<Vec<Expression>> {
        let mut list = vec![];

        if self.peek_token_is(end) {
            self.next_token();
            return Some(list);
        }

        self.next_token();
        if let Some(exp) = self.parse_expression(Precedence::Lowest) {
            list.push(exp);
        }

        while self.peek_token_is(TokenType::Comma) {
            self.next_token();
            self.next_token();
            if let Some(exp) = self.parse_expression(Precedence::Lowest) {
                list.push(exp);
            }
        }

        if !self.expect_peek(end) {
            return None;
        }

        Some(list)
    }

    fn parse_if_expression(&mut self) -> Option<Expression> {
        if let Some(token) = &self.current_token {
            let mut expression = If::new(&token);

            if !self.expect_peek(TokenType::LParen) {
                return None;
            }

            self.next_token();

            expression.condition =
                Some(Box::new(self.parse_expression(Precedence::Lowest).unwrap()));

            if !self.expect_peek(TokenType::RParen) {
                return None;
            }

            if !self.expect_peek(TokenType::LBrace) {
                return None;
            }

            expression.consequence = self.parse_block_statement();

            if self.peek_token_is(TokenType::Else) {
                self.next_token();

                if !self.expect_peek(TokenType::LBrace) {
                    return None;
                }

                expression.alternative = self.parse_block_statement();
            }

            return Some(Expression::If(expression));
        }
        None
    }

    fn parse_call_expression(&mut self, function: Expression) -> Option<Expression> {
        if let Some(token) = &self.current_token {
            let mut expression = CallExpression::new(token, Box::new(function));

            if let Some(args) = self.parse_expression_list(TokenType::RParen) {
                expression.arguments = args;
                return Some(Expression::Call(expression));
            }
        }
        None
    }

    fn parse_block_statement(&mut self) -> Option<BlockStatement> {
        if let Some(token) = &self.current_token {
            let mut block = BlockStatement::new(token);

            self.next_token();

            while self.current_token.is_some() && !self.current_token_is(TokenType::RBrace) {
                if let Some(stmt) = self.parse_statement() {
                    block.statements.push(stmt);
                }
                self.next_token();
            }
            return Some(block);
        }
        None
    }

    fn parse_function_literal(&mut self) -> Option<Expression> {
        if let Some(token) = &self.current_token {
            let mut literal = FunctionLiteral::new(token);

            if !self.expect_peek(TokenType::LParen) {
                return None;
            }

            literal.parameters = self.parse_function_parameters();

            if !self.expect_peek(TokenType::LBrace) {
                return None;
            }

            literal.body = self.parse_block_statement();

            return Some(Expression::Function(literal));
        }
        None
    }

    fn parse_function_parameters(&mut self) -> Vec<Identifier> {
        let mut identifiers = vec![];

        if self.peek_token_is(TokenType::RParen) {
            self.next_token();
            return identifiers;
        }

        self.next_token();

        if let Some(token) = &self.current_token {
            let ident = Identifier::new(token, &token.literal);
            identifiers.push(ident);

            while self.peek_token_is(TokenType::Comma) {
                self.next_token();
                self.next_token();
                if let Some(next_token) = &self.current_token {
                    let ident = Identifier::new(next_token, &next_token.literal);
                    identifiers.push(ident);
                }
            }

            if !self.expect_peek(TokenType::RParen) {
                return vec![];
            }
        }

        identifiers
    }

    fn is_prefix_expression(&self) -> bool {
        if let Some(token) = &self.current_token {
            match token.token_type {
                TokenType::Bang => true,
                TokenType::Minus => true,
                TokenType::If => true,
                TokenType::Function => true,
                TokenType::LParen => true,
                TokenType::LBracket => true,
                _ => false,
            }
        } else {
            false
        }
    }

    fn is_infix_expression(&self) -> bool {
        if let Some(token) = &self.current_token {
            if let Some(peek_token) = &self.peek_token {
                match token.token_type {
                    TokenType::Identifier
                    | TokenType::Int
                    | TokenType::Str
                    | TokenType::True
                    | TokenType::False
                    | TokenType::RParen
                    | TokenType::RBrace
                    | TokenType::RBracket => match peek_token.token_type {
                        TokenType::LParen
                        | TokenType::LBrace
                        | TokenType::LBracket
                        | TokenType::Plus
                        | TokenType::Minus
                        | TokenType::Slash
                        | TokenType::Asterisk
                        | TokenType::Equal
                        | TokenType::NotEqual
                        | TokenType::Lt
                        | TokenType::Gt => true,
                        _ => false,
                    },
                    _ => false,
                }
            } else {
                false
            }
        } else {
            false
        }
    }

    fn peek_precedence(&self) -> Precedence {
        match &self.peek_token {
            Some(token) => match self.precedences.get(&token.token_type) {
                Some(precedence) => *precedence,
                None => Precedence::Lowest,
            },
            None => Precedence::Lowest,
        }
    }

    fn current_precedence(&self) -> Precedence {
        match &self.current_token {
            Some(token) => match self.precedences.get(&token.token_type) {
                Some(precedence) => *precedence,
                None => Precedence::Lowest,
            },
            None => Precedence::Lowest,
        }
    }

    fn current_token_is(&self, token_type: TokenType) -> bool {
        match &self.current_token {
            Some(token) => token.token_type == token_type,
            None => false,
        }
    }

    fn peek_token_is(&self, token_type: TokenType) -> bool {
        match &self.peek_token {
            Some(token) => token.token_type == token_type,
            None => false,
        }
    }

    fn expect_peek(&mut self, token_type: TokenType) -> bool {
        if self.peek_token_is(token_type) {
            self.next_token();
            true
        } else {
            self.peek_error(token_type);
            false
        }
    }

    fn peek_error(&mut self, token_type: TokenType) {
        let peek_token = self.peek_token.clone();
        let err = format!(
            "expected next token to be '{}',but got '{}' instead",
            token_type,
            peek_token.unwrap().token_type
        );
        self.errors.push(err);
    }
}
