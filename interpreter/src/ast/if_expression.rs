use std::fmt;

use super::expression::Expression;
use super::statement::{BlockStatement};
use super::{Node, NodeType};
use crate::token::Token;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct If {
    pub token: Token,
    pub condition: Option<Box<Expression>>,
    pub consequence: Option<BlockStatement>,
    pub alternative: Option<BlockStatement>,
}

impl If {
    pub fn new(token: &Token) -> If {
        If {
            token: token.clone(),
            condition: None,
            consequence: None,
            alternative: None,
        }
    }
}

impl Node for If {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }

    fn node_type(&self) -> NodeType {
        NodeType::If(&self)
    }
}

impl fmt::Display for If {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut string = String::new();

        string.push_str("if");

        if let Some(condition) = self.condition.as_ref().map(std::convert::AsRef::as_ref) {
            string.push_str(&condition.to_string());
        }

        string.push_str(" ");

        if let Some(consequence) = &self.consequence {
            string.push_str(&consequence.to_string());
        }

        if let Some(alternative) = &self.alternative {
            string.push_str("else ");
            string.push_str(&alternative.to_string());
        }

        write!(f, "{}", string)
    }
}
