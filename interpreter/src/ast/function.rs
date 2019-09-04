use std::fmt;

use super::{Node, NodeType};
use super::identifier::Identifier;
use super::statement::BlockStatement;
use crate::token::Token;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FunctionLiteral {
    pub token: Token,
    pub parameters: Vec<Identifier>,
    pub body: Option<BlockStatement>,
}

impl FunctionLiteral {
    pub fn new(token: &Token) -> FunctionLiteral {
        FunctionLiteral {
            token: token.clone(),
            parameters: vec![],
            body: None,
        }
    }
}

impl Node for FunctionLiteral {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }

    fn node_type(&self) -> NodeType {
        NodeType::Function(&self)
    }
}

impl fmt::Display for FunctionLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut string = String::new();
        let parameters = self.parameters.iter().map(std::string::ToString::to_string)
        .collect::<Vec<String>>();

        string.push_str("(");
        string.push_str(&parameters.join(","));
        string.push_str(")");

        if let Some(body) = &self.body {
            string.push_str(&body.to_string());
        }

        write!(f, "{}", &string)
    }
}