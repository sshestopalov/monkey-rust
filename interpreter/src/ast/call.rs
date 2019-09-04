use std::fmt;

use super::{Node, NodeType};
use super::expression::Expression;

use crate::token::Token;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct CallExpression {
    pub token: Token,
    pub function: Box<Expression>,
    pub arguments: Vec<Expression>
}

impl CallExpression {
    pub fn new(token: &Token, function: Box<Expression>) -> CallExpression{
        CallExpression{
            token: token.clone(),
            function: function,
            arguments: vec![],
        }
    }
}

impl Node for CallExpression {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }

    fn node_type(&self) -> NodeType {
        NodeType::Call(&self)
    }
}

impl fmt::Display for CallExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut string = String::new();
        let mut arguments: Vec<String> = vec![];

        for argument in self.arguments.iter() {
            let str_rep = argument.to_string();

            arguments.push(str_rep);
        }

        string.push_str(&self.function.to_string());

        string.push_str("(");
        string.push_str(&arguments.join(", "));
        string.push_str(")");

        write!(f, "{}", &string)
    }
}
