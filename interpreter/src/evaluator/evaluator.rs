use std::cell::RefCell;
use std::rc::Rc;

use crate::ast::array::ArrayLiteral;
use crate::ast::expression::Expression;
use crate::ast::identifier::Identifier;
use crate::ast::if_expression;
use crate::ast::index_expression::IndexExpression;
use crate::ast::statement::{ReturnStatement, Statement};
use crate::ast::{Node, NodeType};
use super::builtins::BUILTINS;
use super::environment::Environment;
use super::object::{Object, BOOLEAN_OBJ, STRING_OBJ};

pub struct Evaluator {
    environment: Rc<RefCell<Environment>>,
}

impl Evaluator {
    pub fn new() -> Evaluator {
        Evaluator {
            environment: Rc::new(RefCell::new(Environment::new())),
        }
    }
    pub fn eval(&mut self, node: &impl Node) -> Option<Object> {
        match node.node_type() {
            NodeType::Program(program) => self.eval_statements(&program.statements),
            NodeType::Statement(stmt) => match stmt {
                Statement::Let(let_statement) => {
                    if let Some(expression) = &let_statement.value {
                        let value = self.eval(expression);

                        return match value.clone() {
                            Some(Object::Error(_)) => value,
                            None => None,
                            Some(v) => {
                                if let Some(identifier) = &let_statement.name {
                                    self.environment.borrow_mut().set(&identifier.value, v);
                                }

                                value.clone()
                            }
                        };
                    }

                    None
                }
                Statement::Expression(expression_stmt) => {
                    if let Some(exp) = &expression_stmt.expression {
                        return self.eval(exp);
                    }
                    None
                }
                Statement::Return(return_statement) => {
                    self.eval_return_statement(&return_statement)
                }
            },
            NodeType::Expression(exp) => match exp {
                Expression::Array(array) => self.eval(array),
                Expression::Index(index_expression) => {
                    Some(self.eval_index_expression(index_expression))
                }
                Expression::Int(int) => self.eval(int),
                Expression::Str(string) => self.eval(string),
                Expression::Bool(boolean) => self.eval(boolean),
                Expression::Prefix(prefix_expression) => {
                    if let Some(right) = &*prefix_expression.right {
                        let evaluated_right = self.eval(right);

                        if let Some(Object::Error(error)) = evaluated_right {
                            return Some(Object::Error(error));
                        }

                        return Some(self.eval_prefix_expression(
                            &prefix_expression.operator,
                            evaluated_right.unwrap(),
                        ));
                    }
                    None
                }
                Expression::Infix(infix_expression) => {
                    if let Some(left) = &*infix_expression.left {
                        if let Some(right) = &*infix_expression.right {
                            let evaluated_left = self.eval(left).unwrap();

                            if let Object::Error(error) = evaluated_left {
                                return Some(Object::Error(error));
                            }

                            let evaluated_right = self.eval(right).unwrap();

                            if let Object::Error(error) = evaluated_right {
                                return Some(Object::Error(error));
                            }

                            return Some(self.eval_infix_expression(
                                &infix_expression.operator,
                                &evaluated_left,
                                &evaluated_right,
                            ));
                        }
                    }
                    None
                }
                Expression::If(if_expression) => self.eval_if_expression(if_expression),
                Expression::Identifier(identifier) => self.eval_identifier(identifier),
                Expression::Function(function_literal) => Some(Object::Function(
                    function_literal.parameters.clone(),
                    function_literal.body.clone().unwrap(),
                    Rc::clone(&self.environment),
                )),
                Expression::Call(call_expression) => {
                    let function_object = self.eval(call_expression.function.as_ref()).unwrap();

                    match function_object {
                        Object::Function(_, _, _) | Object::BuiltinFunction(_) => {
                            let args = self.eval_expressions(call_expression.arguments.clone());
                            self.apply_function(&function_object, args)
                        }
                        Object::Error(_) => Some(function_object),
                        _ => None,
                    }
                }
            },
            NodeType::Integer(integer) => Some(Object::Integer(integer.value)),
            NodeType::Str(string) => Some(Object::Str(string.value.clone())),
            NodeType::Boolean(boolean) => {
                Some(self.native_boolean_to_boolean_object(boolean.value))
            }
            NodeType::Block(block_statement) => self.eval_statements(&block_statement.statements),
            NodeType::Identifier(identifier) => self.eval_identifier(identifier),
            NodeType::Array(array) => Some(self.eval_array(array)),
            _ => None,
        }
    }

    fn eval_statements(&mut self, statements: &Vec<Statement>) -> Option<Object> {
        let mut result = None;

        for stmt in statements.iter() {
            result = self.eval(stmt);

            if let Some(Object::Error(_)) = result {
                break;
            }

            if let Some(Object::ReturnValue(_)) = result {
                break;
            }
        }
        result
    }

    fn native_boolean_to_boolean_object(&self, value: bool) -> Object {
        if value {
            Object::Boolean(true)
        } else {
            Object::Boolean(false)
        }
    }

    fn eval_prefix_expression(&self, operator: &str, right: Object) -> Object {
        match operator {
            "!" => self.eval_bang_operator_expression(right),
            "-" => self.eval_minus_prefix_operator_expression(right),
            _ => Object::Error(format!("unknown operator: {}{}", operator, right)),
        }
    }

    fn eval_bang_operator_expression(&self, right: Object) -> Object {
        let result = match right {
            Object::Boolean(boolean) => {
                if boolean {
                    false
                } else {
                    true
                }
            }
            Object::Null => true,
            _ => false,
        };
        Object::Boolean(result)
    }

    fn eval_minus_prefix_operator_expression(&self, right: Object) -> Object {
        match right {
            Object::Integer(int) => Object::Integer(-int),
            _ => Object::Error(format!("unknown operator: -{}", right.object_type())),
        }
    }

    fn eval_infix_expression(&self, operator: &str, left: &Object, right: &Object) -> Object {
        if left.object_type() != right.object_type() {
            return Object::Error(format!(
                "type mismatch: {} {} {}",
                left.object_type(),
                operator,
                right.object_type()
            ));
        }
        match (left, right) {
            (Object::Integer(l), Object::Integer(r)) => {
                self.eval_infix_integer_expression(operator, *l, *r)
            }
            (Object::Boolean(l), Object::Boolean(r)) => {
                self.eval_infix_boolean_expression(operator, *l, *r)
            }
            (Object::ReturnValue(l), Object::ReturnValue(r)) => {
                self.eval_infix_expression(operator, l.as_ref(), r.as_ref())
            }
            (Object::Str(l), Object::Str(r)) => {
                self.eval_infix_string_expression(operator, l.to_owned(), r.to_owned())
            }
            (_, _) => Object::Error(format!(
                "unknown operator: {}{}{}",
                left.object_type(),
                operator,
                right.object_type()
            )),
        }
    }
    fn eval_infix_integer_expression(&self, operator: &str, left: i64, right: i64) -> Object {
        match operator {
            "+" => Object::Integer(left + right),
            "-" => Object::Integer(left - right),
            "*" => Object::Integer(left * right),
            "/" => Object::Integer(left / right),
            "<" => self.native_boolean_to_boolean_object(left < right),
            ">" => self.native_boolean_to_boolean_object(left > right),
            "==" => self.native_boolean_to_boolean_object(left == right),
            "!=" => self.native_boolean_to_boolean_object(left != right),
            _ => Object::Error(format!("unknown operator: {} {} {}", left, operator, right)),
        }
    }

    fn eval_infix_boolean_expression(&self, operator: &str, left: bool, right: bool) -> Object {
        match operator {
            "==" => self.native_boolean_to_boolean_object(left == right),
            "!=" => self.native_boolean_to_boolean_object(left != right),
            _ => Object::Error(format!(
                "unknown operator: {} {} {}",
                BOOLEAN_OBJ, operator, BOOLEAN_OBJ
            )),
        }
    }

    fn eval_infix_string_expression(&self, operator: &str, left: String, right: String) -> Object {
        match operator {
            "==" => self.native_boolean_to_boolean_object(left == right),
            "!=" => self.native_boolean_to_boolean_object(left != right),
            "+" => {
                let mut new_string = String::new();

                new_string.push_str(&left);
                new_string.push_str(&right);

                Object::Str(new_string)
            }
            _ => Object::Error(format!(
                "unknown operator: {} {} {}",
                STRING_OBJ, operator, STRING_OBJ
            )),
        }
    }

    fn eval_if_expression(&mut self, ie: &if_expression::If) -> Option<Object> {
        let condition = ie
            .condition
            .as_ref()
            .map(std::convert::AsRef::as_ref)
            .unwrap();
        let consequence = ie.consequence.as_ref().unwrap();
        let alternative = ie.alternative.as_ref();
        let evaluated_condition = self.eval(condition);

        if evaluated_condition.as_ref().map_or(false, Object::is_error) {
            return evaluated_condition;
        }

        if evaluated_condition
            .as_ref()
            .map_or(false, |v| self.is_truthy(v))
        {
            return self.eval(consequence);
        } else if alternative.is_some() {
            return self.eval(alternative.unwrap());
        }

        Some(Object::Null)
    }

    fn eval_return_statement(&mut self, return_statement: &ReturnStatement) -> Option<Object> {
        if let Some(return_value) = &return_statement.return_value {
            let value = self.eval(return_value).unwrap();

            if let Object::Error(error) = value {
                return Some(Object::Error(error));
            }

            return Some(Object::ReturnValue(Box::new(value)));
        }
        None
    }

    fn eval_identifier(&self, identifier: &Identifier) -> Option<Object> {
        if let Some(value) = self.environment.borrow().get(&identifier.value) {
            return Some(value);
        }

        if let Some((_, built_in)) = BUILTINS
            .iter()
            .find(|(key, _)| key.as_ref() == identifier.value)
        {
            return Some(built_in.clone());
        }

        Some(Object::Error(format!(
            "identifier not found: {}",
            identifier.value
        )))
    }

    fn eval_expressions(&mut self, expressions: Vec<Expression>) -> Vec<Option<Object>> {
        let mut result = vec![];

        for expression in expressions {
            let evaluated = self.eval(&expression);
            if let Some(Object::Error(_)) = evaluated {
                result = vec![evaluated];

                break;
            }

            result.push(evaluated);
        }

        result
    }

    fn apply_function(&mut self, function: &Object, args: Vec<Option<Object>>) -> Option<Object> {
        if let Object::Function(params, body, function_env) = function {
            if params.len() != args.len() {
                return Some(Object::Error(format!(
                    "Wrong number of arguments supplied ({})/({})",
                    args.len(),
                    params.len()
                )));
            }

            let old_env = Rc::clone(&self.environment);
            let mut new_env = Environment::new_enclosed(Rc::clone(&function_env));
            let zipped = params.into_iter().zip(args.into_iter());

            for (param, arg) in zipped {
                new_env.set(&param.value, arg.unwrap());
            }
            // use new environment for evalutating function body
            self.environment = Rc::new(RefCell::new(new_env));

            let evaluated = self.eval(body);

            // swith back to old env
            self.environment = old_env;

            return evaluated;
        }

        if let Object::BuiltinFunction(built_in) = function {
            return Some((built_in.func)(&args));
        }

        Some(Object::Error(format!(
            "not a function: {}",
            function.object_type()
        )))
    }

    fn eval_array_index_expression(&mut self, left: Object, index: Object) -> Object {
        match (left, index) {
            (Object::Array(elements), Object::Integer(idx)) => {
                if idx < 0 || idx > elements.len() as i64 {
                    return Object::Null;
                }

                elements[idx as usize].clone()
            }
            _ => Object::Null,
        }
    }

    fn eval_array(&mut self, array_object: &ArrayLiteral) -> Object {
        let elements = self.eval_expressions(array_object.elements.clone());

        if let Some(Object::Error(_)) = elements[0] {
            return elements[0].clone().unwrap();
        }

        Object::Array(elements.into_iter().map(|v| v.unwrap()).collect())
    }

    fn eval_index_expression(&mut self, index_expression: &IndexExpression) -> Object {
        let left = self.eval(&*index_expression.left).unwrap();

        if left.is_error() {
            return left;
        }

        let index = self.eval(&*index_expression.index).unwrap();

        if index.is_error() {
            return index;
        }

        if left.is_array() && index.is_integer() {
            return self.eval_array_index_expression(left, index);
        }

        Object::Error(format!(
            "index operator not supported: {}",
            left.object_type()
        ))
    }

    fn is_truthy(&self, obj: &Object) -> bool {
        match obj {
            Object::Boolean(boolean) => *boolean,
            Object::Null => false,
            _ => true,
        }
    }
}
