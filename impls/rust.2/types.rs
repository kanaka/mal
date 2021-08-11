#[derive(Debug)]
pub enum MalValue {
    MalSymbol(String),
    MalString(String),
    MalInteger(i32),
    MalList(Vec<MalValue>),
    MalVector(Vec<MalValue>)
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
                            string[1..string.len() - 2]
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
            }
        }
    }

}

#[derive(Debug)]
pub enum MalError {
    ParseError(String)
}