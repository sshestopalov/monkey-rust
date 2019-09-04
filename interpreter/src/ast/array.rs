use std::fmt;

use crate::token::Token;
use super::{Node, NodeType};
use super::expression::Expression;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ArrayLiteral {
    pub token: Token,
    pub elements: Vec<Expression>
}

impl ArrayLiteral {
    pub fn new(token: &Token) -> ArrayLiteral {
        ArrayLiteral {token: token.clone(), elements: vec![]}
    }
}

impl Node for ArrayLiteral {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }

    fn node_type(&self) -> NodeType {
        NodeType::Array(self)
    }
}

impl fmt::Display for ArrayLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut string = String::from("[");
        let elements_string: Vec<String> = self.elements.iter().map(|e| e.to_string()).collect();

        string.push_str(&elements_string.join(", "));

        string.push_str("]");

        write!(f, "{}", string)
    }
}