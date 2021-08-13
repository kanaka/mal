pub struct Environment {
    symbols: std::collections::HashMap<String, super::types::MalValue>
}

impl Environment {
    pub fn new() -> Environment {
        return Environment
        {
            symbols: std::collections::HashMap::<String, super::types::MalValue>::new()
        }
    }

    pub fn lookup_symbol(&self, symbol: String) -> Option<super::types::MalValue> {
        if let Some(value) = self.symbols.get(&symbol) {
            return Some(value.clone());
        }
        return None;
    }

    pub fn add_symbol(&mut self, symbol: String, value: super::types::MalValue) {
        self.symbols.insert(symbol, value);
    }
}