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
    pub fn new_with_binds(outer: Option<&'a Env>, binds: &[MalType], exprs: &[MalType]) -> Self {
        let mut data = HashMap::new();
        for (k, v) in binds.iter().zip(exprs.iter()) {
            match &k {
                MalType::Symbol(sk) => {
                    data.insert(sk.to_string(), v.clone());
                }
                _ => {}
            }
        }
        Env { outer, data }
    }

    pub fn new(outer: Option<&'a Env>) -> Self {
        Self::new_with_binds(outer, &[], &[])
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
