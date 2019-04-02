use super::types::MalType;
use std::collections::HashMap;
use std::error::Error;
use std::fmt;

#[derive(Debug)]
struct EnvError {
    err: String,
}

impl EnvError {
    fn new(err: &str) -> Box<Error> {
        Box::new(EnvError {
            err: err.to_owned(),
        })
    }
}

impl Error for EnvError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        None
    }
}

impl fmt::Display for EnvError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Environment error: {}", self.err)
    }
}

pub struct Env<'a> {
    outer: Option<&'a Env<'a>>,
    data: HashMap<String, MalType>,
}

impl<'a> Env<'a> {
    pub fn new(outer: Option<&'a Env>) -> Self {
        Env {
            outer: outer,
            data: HashMap::new(),
        }
    }

    pub fn set(&mut self, symbol: &str, val: &MalType) {
        self.data.insert(symbol.to_string(), val.clone());
    }

    pub fn find(&self, symbol: &str) -> Option<&Env> {
        match self.data.get(symbol) {
            Some(_) => Some(self),
            None => match &self.outer {
                Some(o) => o.find(symbol),
                None => None,
            },
        }
    }

    pub fn get(&self, symbol: &str) -> super::types::Result {
        match self.find(symbol) {
            Some(e) => e
                .data
                .get(symbol)
                .map(|v| v.clone())
                .ok_or(EnvError::new(&format!("Failed to find {} in env", symbol))),
            None => Err(EnvError::new(&format!("'{}' not found.", symbol))),
        }
    }
}
