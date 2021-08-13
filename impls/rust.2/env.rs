use std::collections::HashMap;
use super::types::{MalValue, MalResult, MalError};
use std::rc::Rc;
use std::cell::RefCell;

#[derive(Debug, Clone)]
pub struct Environment {
    data: RefCell<HashMap<String, MalValue>>,
    outer: Option<Rc<Environment>>
}

impl Environment {
    pub fn new(outer: Option<Rc<Environment>>) -> Environment {
        return Environment
        {
            data: RefCell::new(HashMap::<String, MalValue>::new()),
            outer
        }
    }

    pub fn lookup_symbol(&self, symbol: String) -> Option<MalValue> {
        if let Some(value) = self.data.borrow().get(&symbol) {
            return Some(value.clone());
        }
        return None;
    }

    pub fn set(&self, key: MalValue, value: MalValue) -> MalResult {
        match key {
            MalValue::MalSymbol(ref symbol) => {
                self.data.borrow_mut().insert(symbol.to_string(), value.clone());
                return Ok(value);
            },
            _ => return Err(MalError::EvalError(String::from("Not string key")))
        }
        
    }

    pub fn find(&self, symbol: &str) -> Option<Rc<Environment>> {
        return match (self.data.borrow().contains_key(symbol), self.outer.clone()) {
            (true, _) => Some(Rc::new(self.clone())),
            (false, Some(o)) => o.find(symbol),
            _ => None
        }
    }

    pub fn get (&self, key: MalValue) -> MalResult {
        return match key {
            MalValue::MalSymbol(ref symbol) => match self.find(symbol) {
                Some(e) => Ok(e.data.borrow().get(symbol).unwrap().clone()),
                _ => Err(MalError::EvalError(String::from("Notfound")))
            },
            _ => Err(MalError::EvalError(String::from("Not symbol key")))
        }
    }
}