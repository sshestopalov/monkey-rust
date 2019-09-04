use std::fmt;

use super::{Node, NodeType};
use crate::token::Token;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl Identifier {
    pub fn new(token: &Token, value: &str) -> Identifier {
        Identifier {
            token: token.clone(),
            value: value.to_owned(),
        }
    }
}

impl Node for Identifier {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }

    fn node_type(&self) -> NodeType {
        NodeType::Identifier(&self)
    }
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}
