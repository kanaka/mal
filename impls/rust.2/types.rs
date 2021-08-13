#[derive(Debug, PartialEq, Clone)]
pub enum MalValue {
    MalSymbol(String),
    MalString(String),
    MalInteger(i32),
    MalList(Vec<MalValue>),
    MalVector(Vec<MalValue>),
    MalKeyword(String),
    MalHashmap(Vec<MalValue>, Vec<MalValue>),
    MalFunction(fn(Vec<MalValue>) -> MalValue)
}

pub type MalResult = Result<MalValue, MalError>;

impl std::hash::Hash for MalValue {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            MalValue::MalHashmap(keys, values) => {
                keys.hash(state);
                values.hash(state);
            },
            MalValue::MalInteger(int) => {
                int.hash(state);
            },
            MalValue::MalKeyword(keyword) => {
                keyword.hash(state);
            },
            MalValue::MalList(list) => {
                list.hash(state);
            },
            MalValue::MalString(string) => {
                string.hash(state);
            },
            MalValue::MalSymbol(symbol) => {
                symbol.hash(state);
            },
            MalValue::MalVector(vector) =>{
                vector.hash(state);
            },
            MalValue::MalFunction(symbol) => {
                symbol.hash(state);
            }
        }
    }
}

impl MalValue {
    pub fn inspect(&self, print_readably: bool) -> String {
        match self {
            MalValue::MalSymbol(symbol) => {
                return symbol.to_string();
            }
            MalValue::MalString(string) => {
                if string.len() == 2 {
                    return string.to_string();
                }
                if print_readably {
                    return format!("\"{}\"", 
                            string[1..string.len() - 1]
                                 .replace('\\', "\\\\")
                                 .replace('"', "\\\"")
                                 .replace('\n', "\\n"));
                    
                }
                return string.to_string();
            },
            MalValue::MalInteger(int) => {
                return int.to_string();
            },
            MalValue::MalList(list) => {
                let mut output = String::from('(');

                let mut first_token = true;
                for token in list {
                    if !first_token {
                        output += " ";
                    } else {
                        first_token = false;
                    }
                    output += &token.inspect(print_readably);
                }
                output += &String::from(')');
                return output;
            },
            MalValue::MalVector(list) => {
                let mut output = String::from('[');

                let mut first_token = true;
                for token in list {
                    if !first_token {
                        output += " ";
                    } else {
                        first_token = false;
                    }
                    output += &token.inspect(print_readably);
                }
                output += &String::from(']');
                return output;
            },
            MalValue::MalKeyword(keyword) => {
                return format!(":{}", keyword);
            },
            MalValue::MalHashmap(keys, values) => {
                let mut output = String::from('{');

                let mut first_token = true;
                for (key, value) in keys.iter().zip(values.iter()) {
                    if !first_token {
                        output += " ";
                    } else {
                        first_token = false;
                    }
                    output += &format!("{} {}", key.inspect(print_readably), value.inspect(print_readably));
                }

                output += &String::from('}');
                return output;
            },
            MalValue::MalFunction(_symbol) => {
                return "#<function>".to_string();
            }
        }
    }

}

#[derive(Debug)]
pub enum MalError {
    ParseError(String),
    EvalError(String)
}