use super::types::{list, list_from_slice, MalError, MalResult, MalValue};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Environment {
    data: RefCell<HashMap<String, MalValue>>,
    outer: Option<Rc<Environment>>,
}

impl Environment {
    pub fn new(
        outer: Option<Rc<Environment>>,
        binds: Option<Vec<MalValue>>,
        exprs: Option<Vec<MalValue>>,
    ) -> Environment {
        let env = Environment {
            data: RefCell::new(HashMap::<String, MalValue>::new()),
            outer,
        };

        match (binds, exprs) {
            (Some(b), Some(e)) => {
                for i in 0..b.len() {
                    let bind = b[i].clone();

                    if let MalValue::MalSymbol(s) = bind.clone() {
                        if s == "&" {
                            
                            if e.len() > i {
                                let expr = e[i].clone();
                                env.set(b[i + 1].clone(), list_from_slice(&e[i..e.len()]));
                                break;   
                            } else {
                                env.set(b[i + 1].clone(), list(Vec::<MalValue>::new()));
                            }

                        } else if e.len() > i {
                            env.set(bind, e[i].clone());
                        }
                    } else {
                        panic!("Non-symbol key");
                    }
                }
            }
            _ => {}
        };

        return env;
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
                self.data
                    .borrow_mut()
                    .insert(symbol.to_string(), value.clone());
                return Ok(value);
            }
            _ => return Err(MalError::EvalError(String::from("Not string key"))),
        }
    }

    pub fn find(&self, symbol: &str) -> Option<Rc<Environment>> {
        return match (self.data.borrow().contains_key(symbol), self.outer.clone()) {
            (true, _) => Some(Rc::new(self.clone())),
            (false, Some(o)) => o.find(symbol),
            _ => None,
        };
    }

    pub fn get(&self, key: MalValue) -> MalResult {
        return match key {
            MalValue::MalSymbol(ref symbol) => match self.find(symbol) {
                Some(e) => Ok(e.data.borrow().get(symbol).unwrap().clone()),
                _ => Err(MalError::EvalError(String::from("Not found"))),
            },
            _ => Err(MalError::EvalError(String::from("Not symbol key"))),
        };
    }
}
