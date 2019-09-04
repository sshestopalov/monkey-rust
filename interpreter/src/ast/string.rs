use crate::ast::{Node, NodeType};
use crate::token::Token;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct StringLiteral {
    pub token: Token,
    pub value: String,
}

impl StringLiteral {
    pub fn new(token: &Token, value: &str) -> StringLiteral {
        StringLiteral {
            token: token.clone(),
            value: value.to_owned(),
        }
    }
}

impl Node for StringLiteral {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }

    fn node_type(&self) -> NodeType {
        NodeType::Str(&self)
    }
}
