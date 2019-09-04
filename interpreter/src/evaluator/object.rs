use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

use super::environment::Environment;
use crate::ast::identifier::Identifier;
use crate::ast::statement::BlockStatement;

pub const INTEGER_OBJ: &'static str = "INTEGER";
pub const STRING_OBJ: &'static str = "STRING";
pub const BOOLEAN_OBJ: &'static str = "BOOLEAN";
pub const NULL_OBJ: &'static str = "NULL";
pub const RETURN_VALUE_OBJ: &'static str = "RETURN_VALUE";
pub const ERROR_OBJ: &'static str = "ERROR_OBJ";
pub const FUNCTION_OBJECT: &'static str = "FUNCTION";
pub const BUILTIN: &'static str = "BUILTIN";
pub const ARRAY_OBJ: &'static str = "ARRAY";

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Integer(i64),
    Str(String),
    Boolean(bool),
    ReturnValue(Box<Object>),
    Error(String),
    Function(Vec<Identifier>, BlockStatement, Rc<RefCell<Environment>>),
    BuiltinFunction(BuiltinFunction),
    Array(Vec<Object>),
    Null,
}

impl Object {
    pub fn is_error(&self) -> bool {
        self.object_type() == ERROR_OBJ
    }

    pub fn is_array(&self) -> bool {
        self.object_type() == ARRAY_OBJ
    }

    pub fn is_integer(&self) -> bool {
        self.object_type() == INTEGER_OBJ
    }

    pub fn object_type(&self) -> &str {
        match self {
            Object::Integer(_) => INTEGER_OBJ,
            Object::Str(_) => STRING_OBJ,
            Object::Boolean(_) => BOOLEAN_OBJ,
            Object::ReturnValue(_) => RETURN_VALUE_OBJ,
            Object::Error(_) => ERROR_OBJ,
            Object::Function(_, _, _) => FUNCTION_OBJECT,
            Object::BuiltinFunction(_) => BUILTIN,
            Object::Array(_) => ARRAY_OBJ,
            Object::Null => NULL_OBJ,
        }
    }
}

#[derive(Clone)]
pub struct BuiltinFunction {
    pub func: fn(&[Option<Object>]) -> Object,
}

impl fmt::Debug for BuiltinFunction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "builtin function")
    }
}

impl PartialEq for BuiltinFunction {
    fn eq(&self, other: &BuiltinFunction) -> bool {
        self.func as usize == other.func as usize
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Object::Integer(int) => int.to_string(),
                Object::Str(string) => string.to_string(),
                Object::Boolean(bool) => bool.to_string(),
                Object::ReturnValue(return_value) => return_value.to_string(),
                Object::Error(err) => err.to_string(),
                Object::Function(parameters, body, _) => {
                    let mut string = String::new();

                    let string_params: Vec<String> = parameters
                        .iter()
                        .map(std::string::ToString::to_string)
                        .collect();

                    string.push_str("fn");
                    string.push_str("(");
                    string.push_str(&string_params.join(", "));
                    string.push_str(") {\n");
                    string.push_str(&format!("{}", body));
                    string.push_str("\n}");

                    string
                }
                Object::BuiltinFunction(_) => "builtin function".to_owned(),
                Object::Array(array) => {
                    let mut string = String::from("[");

                    let string_elements: Vec<String> =
                        array.iter().map(std::string::ToString::to_string).collect();
                    string.push_str(&string_elements.join(", "));
                    string.push(']');

                    string
                }
                Object::Null => "null".to_owned(),
            }
        )
    }
}
