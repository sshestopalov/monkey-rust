use crate::ast::{Node, NodeType};
use crate::token::Token;

#[derive(Debug, Eq, Clone)]
pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

impl IntegerLiteral {
    pub fn new(token: &Token, value: &str) -> IntegerLiteral {
        let parsed_value = value.parse::<i64>().unwrap();

        IntegerLiteral {
            token: token.clone(),
            value: parsed_value,
        }
    }
}

impl PartialEq for IntegerLiteral {
    fn eq(&self, other: &IntegerLiteral) -> bool {
        self.token == other.token && self.value == other.value
    }
}

impl Node for IntegerLiteral {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }

    fn node_type(&self) -> NodeType {
        NodeType::Integer(&self)
    }
}
