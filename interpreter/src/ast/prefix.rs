use super::expression::Expression;
use super::{Node, NodeType};
use crate::token::Token;

#[derive(Debug, Eq, Clone)]
pub struct Prefix {
    pub token: Token,
    pub operator: String,
    pub right: std::boxed::Box<Option<Expression>>,
}

impl Prefix {
    pub fn new(token: &Token, operator: &str, right: Option<Expression>) -> Prefix {
        Prefix {
            token: token.clone(),
            operator: operator.to_owned(),
            right: Box::new(right),
        }
    }
}

impl PartialEq for Prefix {
    fn eq(&self, other: &Prefix) -> bool {
        self.token == other.token && self.operator == other.operator && self.right == other.right
    }
}

impl Node for Prefix {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }

    fn node_type(&self) -> NodeType {
        NodeType::Prefix(&self)
    }
}