use super::env::Environment;
use std::rc::Rc;

pub fn bool(value: bool) -> MalValue {
    MalValue::MalBool(value)
}

pub fn func(f: fn(Vec<MalValue>) -> MalResult) -> MalValue {
    MalValue::MalFunction(f, Rc::new(MalValue::MalNil))
}

pub fn list(vals: Vec<MalValue>) -> MalValue {
    MalValue::MalList(vals)
}

pub fn list_from_slice(vals: &[MalValue]) -> MalValue {
    let mut result = Vec::<MalValue>::new();

    for val in vals {
        result.push(val.clone());
    }

    MalValue::MalList(result)
}

#[derive(Debug, Clone)]
pub enum MalValue {
    MalSymbol(String),
    MalString(String),
    MalInteger(i32),
    MalList(Vec<MalValue>),
    MalVector(Vec<MalValue>),
    MalKeyword(String),
    MalHashmap(Vec<MalValue>, Vec<MalValue>),
    MalFunction(fn(Vec<MalValue>) -> MalResult, Rc<MalValue>),
    MalFunc {
        eval: fn(ast: MalValue, env: Rc<Environment>) -> MalResult,
        ast: Rc<MalValue>,
        env: Rc<Environment>,
        params: Rc<MalValue>,
    },
    MalNil,
    MalBool(bool),
}

pub type MalResult = Result<MalValue, MalError>;

impl std::hash::Hash for MalValue {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            MalValue::MalHashmap(keys, values) => {
                keys.hash(state);
                values.hash(state);
            }
            MalValue::MalInteger(int) => {
                int.hash(state);
            }
            MalValue::MalKeyword(keyword) => {
                keyword.hash(state);
            }
            MalValue::MalList(list) => {
                list.hash(state);
            }
            MalValue::MalString(string) => {
                string.hash(state);
            }
            MalValue::MalSymbol(symbol) => {
                symbol.hash(state);
            }
            MalValue::MalVector(vector) => {
                vector.hash(state);
            }
            MalValue::MalFunction(symbol, _) => {
                symbol.hash(state);
            }
            MalValue::MalBool(b) => {
                b.hash(state);
            }
            MalValue::MalNil => {
                "nil".hash(state);
            }
            MalValue::MalFunc { ast, .. } => {
                ast.hash(state);
            }
        }
    }
}

impl PartialEq for MalValue {
    fn eq(&self, other: &MalValue) -> bool {
        match (self, other) {
            (MalValue::MalNil, MalValue::MalNil) => true,
            (MalValue::MalBool(ref a), MalValue::MalBool(ref b)) => a == b,
            (MalValue::MalInteger(ref a), MalValue::MalInteger(ref b)) => a == b,
            (MalValue::MalString(ref a), MalValue::MalString(ref b)) => a == b,
            (MalValue::MalSymbol(ref a), MalValue::MalSymbol(ref b)) => a == b,
            (MalValue::MalList(ref a), MalValue::MalList(ref b))
            | (MalValue::MalVector(ref a), MalValue::MalVector(ref b))
            | (MalValue::MalList(ref a), MalValue::MalVector(ref b))
            | (MalValue::MalVector(ref a), MalValue::MalList(ref b)) => a == b,
            (MalValue::MalHashmap(ref a1, ref a2), MalValue::MalHashmap(ref b1, b2)) => {
                a1 == a2 && b1 == b2
            }
            (MalValue::MalFunc { .. }, MalValue::MalFunc { .. }) => false,
            (MalValue::MalKeyword(ref k1), MalValue::MalKeyword(ref k2)) => k1 == k2,
            _ => false,
        }
    }
}

impl MalValue {
    pub fn as_vec(&self) -> Option<Vec<MalValue>> {
        return match self {
            MalValue::MalList(list) => Some(list.clone()),
            MalValue::MalVector(list) => Some(list.clone()),
            _ => None,
        };
    }

    pub fn is_truthy(&self) -> bool {
        return match self {
            MalValue::MalBool(b) => *b,
            MalValue::MalNil => false,
            _ => true,
        };
    }

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
                    if string.len() == 0 {
                        return format!("\"{}\"", string);
                    }
                    return format!(
                        "\"{}\"",
                        string
                            .replace('\\', "\\\\")
                            .replace('"', "\\\"")
                            .replace('\n', "\\n")
                            .replace('[', "\\[")
                            .replace(']', "\\]")
                    );
                }
                return string.to_string();
            }
            MalValue::MalInteger(int) => {
                return int.to_string();
            }
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
            }
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
            }
            MalValue::MalKeyword(keyword) => {
                return format!(":{}", keyword);
            }
            MalValue::MalHashmap(keys, values) => {
                let mut output = String::from('{');

                let mut first_token = true;
                for (key, value) in keys.iter().zip(values.iter()) {
                    if !first_token {
                        output += " ";
                    } else {
                        first_token = false;
                    }
                    output += &format!(
                        "{} {}",
                        key.inspect(print_readably),
                        value.inspect(print_readably)
                    );
                }

                output += &String::from('}');
                return output;
            }
            MalValue::MalFunction(_, _) => {
                return "#<function>".to_string();
            }
            MalValue::MalFunc { .. } => {
                return "#<function>".to_string();
            }
            MalValue::MalBool(b) => return b.to_string(),
            MalValue::MalNil => return "nil".to_string(),
        }
    }
}

#[derive(Debug)]
pub enum MalError {
    ParseError(String),
    EvalError(String),
}
