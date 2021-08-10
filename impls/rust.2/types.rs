#[derive(Debug)]
pub enum MalValue {
    MalString(String),
    MalInteger(i32),
    MalList(Vec<MalValue>)
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
                for token in list {
                    output += &token.inspect();
                }
                output += &String::from(')');
                return output;
            }
        }
    }

}