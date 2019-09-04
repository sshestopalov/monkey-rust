use super::{Node, NodeType};
use crate::token::Token;

#[derive(Debug, PartialEq,Eq, Clone)]
pub struct Boolean {
    pub token: Token,
    pub value: bool
}

impl Boolean {
    pub fn new(token: &Token, value: bool) -> Boolean {
        Boolean {
            token: token.clone(),
            value: value
        }
    }
}

impl Node for Boolean {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }

    fn node_type(&self) -> NodeType {
        NodeType::Boolean(&self)
    }
}