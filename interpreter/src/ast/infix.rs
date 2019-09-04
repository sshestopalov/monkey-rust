use std::boxed::Box;

use super::expression::Expression;
use super::{Node, NodeType};
use crate::token::Token;

#[derive(Debug, PartialEq,Eq, Clone)]
pub struct Infix {
    pub token: Token,
    pub left: Box<Option<Expression>>,
    pub operator: String,
    pub right: Box<Option<Expression>>,
}

impl Infix {
    pub fn new(
        token: &Token,
        left: Option<Expression>,
        operator: &str,
        right: Option<Expression>,
    ) -> Infix {
        Infix {
            token: token.clone(),
            left: Box::new(left),
            operator: operator.to_owned(),
            right: Box::new(right),
        }
    }
}

impl Node for Infix {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }

    fn node_type(&self) -> NodeType {
        NodeType::Infix(&self)
    }
}