#[derive(Debug)]
pub enum MalValue {
    MalString(String),
    MalInteger(i32),
    MalList(Vec<MalValue>),
    MalVector(Vec<MalValue>)
}

impl MalValue {
    pub fn inspect(&self) -> String {
        match self {
            MalValue::MalString(string) => {
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
                    output += &token.inspect();
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
                    output += &token.inspect();
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