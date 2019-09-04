pub mod program;
pub mod statement;
pub mod identifier;
pub mod expression;
pub mod int;
pub mod string;
pub mod array;
pub mod boolean;
pub mod prefix;
pub mod infix;
pub mod if_expression;
pub mod index_expression;
pub mod function;
pub mod call;

pub trait Node {
   fn token_literal(&self) -> &str;
   fn node_type(&self) -> NodeType;
}

#[derive(Debug)]
pub enum NodeType<'a> {
   Statement(&'a statement::Statement),
   Expression(&'a expression::Expression),
   Identifier(&'a identifier::Identifier),
   Program(&'a program::Program),
   Function(&'a function::FunctionLiteral),
   Call(&'a call::CallExpression),
   Block(&'a statement::BlockStatement),
   Prefix(&'a prefix::Prefix),
   Infix(&'a infix::Infix),
   Boolean(&'a boolean::Boolean),
   Integer(&'a int::IntegerLiteral),
   Str(&'a string::StringLiteral),
   Array(&'a array::ArrayLiteral),
   If(&'a if_expression::If),
   Index(&'a index_expression::IndexExpression)
}