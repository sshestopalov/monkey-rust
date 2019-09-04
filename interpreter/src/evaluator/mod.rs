#[cfg(test)]
mod tests;

mod environment;
mod builtins;
mod evaluator;
pub mod object;

pub use evaluator::Evaluator;