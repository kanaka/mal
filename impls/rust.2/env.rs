use std::collections::HashMap;
use super::types::MalValue;

pub struct Environment {
    symbols: HashMap<String, MalValue>
}

impl Environment {
    pub fn new() -> Environment {
        return Environment
        {
            symbols: HashMap::<String, MalValue>::new()
        }
    }

    pub fn lookup_symbol(&self, symbol: String) -> Option<MalValue> {
        if let Some(value) = self.symbols.get(&symbol) {
            return Some(value.clone());
        }
        return None;
    }

    pub fn add_symbol(&mut self, symbol: String, value: MalValue) {
        self.symbols.insert(symbol, value);
    }
}