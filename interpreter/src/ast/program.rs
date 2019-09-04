use std::fmt;

use super::{statement, Node, NodeType};

#[derive(Debug, Clone)]
pub struct Program {
    pub statements: Vec<statement::Statement>,
}

impl Program {
    pub fn new() -> Program {
        Program { statements: vec![] }
    }
}

impl Node for Program {
    fn token_literal(&self) -> &str {
        if !self.statements.is_empty() {
            self.statements[0].token_literal()
        } else {
            ""
        }
    }

    fn node_type(&self) -> NodeType {
        NodeType::Program(&self)
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut string = String::new();

        for s in &self.statements {
            string.push_str(&s.to_string());
        }

        write!(f, "{}", string)
    }
}
