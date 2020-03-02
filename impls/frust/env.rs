use super::types::MalType;
use super::{MalError, MalResult};
use std::collections::HashMap;

#[derive(Debug)]
pub(crate) struct Env {
    lookup: HashMap<String, MalType>,
    pub count: usize,
}

impl Env {
    pub fn new() -> Self {
        let lookup = HashMap::new();

        Env { lookup, count: 0 }
    }

    pub fn lookup(&self, s: &str) -> MalResult {
        match self.lookup.get(s) {
            Some(v) => Ok(v.clone()),
            None => Err(MalError::SymbolNotFound(s.to_owned())),
        }
    }
}
