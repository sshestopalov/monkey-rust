use std::fmt;
use super::{Node, NodeType};
use super::expression::Expression;
use crate::token::Token;

#[derive(Debug, Eq, Clone)]
pub struct IndexExpression {
    pub token: Token,
    pub left: Box<Expression>,
    pub index: Box<Expression>,
}

impl IndexExpression {
    pub fn new(token: &Token, left: Expression, index: Expression) -> Self {
        IndexExpression {
            token: token.clone(),
            left: Box::new(left),
            index: Box::new(index),
        }
    }
}

impl PartialEq for IndexExpression {
    fn eq(&self, other: &IndexExpression) -> bool {
        self.token == other.token
            && self.left == other.left
            && self.index == other.index
    }
}

impl Node for IndexExpression {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }

    fn node_type(&self) -> NodeType {
        NodeType::Index(self)
    }
}

impl fmt::Display for IndexExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut string = String::new();

        string.push('(');
        string.push_str(&self.left.to_string());
        string.push('[');
        string.push_str(&self.index.to_string());
        string.push(']');
        string.push(')');

        write!(f, "{}", string)
    }
}
