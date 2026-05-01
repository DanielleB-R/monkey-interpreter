use crate::object::Object;
use std::collections::HashMap;

#[derive(Debug, Default, Clone, PartialEq)]
pub struct Environment {
    store: HashMap<String, Object>,
}

impl Environment {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn with_enclosed(outer: &Environment) -> Self {
        Self {
            store: outer.store.clone(),
        }
    }

    pub fn get(&self, name: &str) -> Option<Object> {
        self.store.get(name).cloned()
    }

    pub fn set(&mut self, name: &str, val: Object) {
        self.store.insert(name.to_owned(), val);
    }
}
