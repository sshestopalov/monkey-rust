use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

use super::object::Object;

#[derive(Debug, PartialEq)]
pub struct Environment {
    store: HashMap<String, Object>,
    outer: Option<Rc<RefCell<Environment>>>
}

impl Environment {
    pub fn new() -> Environment {
        Environment{
            store: HashMap::new(),
            outer: None
        }
    }

    pub fn new_enclosed(outer: Rc<RefCell<Environment>>) -> Environment {
        Environment {
            store: HashMap::new(),
            outer: Some(outer)
        }
    }

    pub fn get(&self, name: &str) -> Option<Object> {
        let value = self.store.get(name);

        match value {
            Some(v) => Some(v.clone()),
            None => match &self.outer {
                Some(env) => env.borrow().get(name),
                None => None,
            }
        }
    }

    pub fn set(&mut self, name: &str, value: Object) -> Option<Object> {
        self.store.insert(name.to_owned(), value)
    }
}