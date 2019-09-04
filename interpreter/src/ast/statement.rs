use std::fmt;

use super::{expression::Expression, identifier::Identifier, Node, NodeType};
use crate::token::Token;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
}

impl Node for Statement {
    fn token_literal(&self) -> &str {
        match self {
            Statement::Let(l) => &l.token.literal,
            Statement::Return(l) => &l.token.literal,
            Statement::Expression(l) => &l.token.literal,
        }
    }

    fn node_type(&self) -> NodeType {
        NodeType::Statement(&self)
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::Let(s) => write!(f, "{}", s.to_string()),
            Statement::Return(s) => write!(f, "{}", s.to_string()),
            Statement::Expression(s) => write!(
                f,
                "{}",
                match &s.expression {
                    Some(e) => e.to_string(),
                    None => String::new(),
                }
            ),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct LetStatement {
    pub token: Token,
    pub name: Option<Identifier>,
    pub value: Option<Expression>,
}

impl LetStatement {
    pub fn new(token: &Token) -> LetStatement {
        LetStatement {
            token: token.clone(),
            name: None,
            value: None,
        }
    }
}

impl fmt::Display for LetStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{} {} = {};",
            self.token.literal,
            match &self.name {
                Some(name) => name.to_string(),
                None => String::new(),
            },
            match &self.value {
                Some(value) => value.to_string(),
                None => String::new(),
            },
        )
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ReturnStatement {
    pub token: Token,
    pub return_value: Option<Expression>,
}

impl ReturnStatement {
    pub fn new(token: &Token) -> ReturnStatement {
        ReturnStatement {
            token: token.clone(),
            return_value: None,
        }
    }
}

impl fmt::Display for ReturnStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{} {};",
            self.token.literal,
            match &self.return_value {
                Some(return_value) => return_value.to_string(),
                None => String::new(),
            }
        )
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Option<Expression>,
}

impl ExpressionStatement {
    pub fn new(token: &Token) -> ExpressionStatement {
        ExpressionStatement {
            token: token.clone(),
            expression: None,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Statement>,
}

impl BlockStatement {
    pub fn new(token: &Token) -> BlockStatement {
        BlockStatement {
            token: token.clone(),
            statements: vec![],
        }
    }
}

impl Node for BlockStatement {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }

    fn node_type(&self) -> NodeType {
        NodeType::Block(&self)
    }
}

impl fmt::Display for BlockStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut string = String::new();

        for s in &self.statements {
            string.push_str(&s.to_string());
        }

        write!(f, "{}", string)
    }
}
