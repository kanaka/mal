pub struct Reader {
    position: usize,
    tokens: Vec<String>
}

impl Reader {
    fn new(tokens: Vec<String>) -> Reader {
        return Reader{
            position: 0,
            tokens: tokens
        };
    }

    pub fn peek(&self) -> Option<String> {
        if self.position >= self.tokens.len() {
            return None;
        }
        return Some(self.tokens[self.position].to_owned());
    }

    pub fn next(&mut self) -> Option<String> {
        let token = self.peek();
        self.position += 1;
        return token;
    }

    pub fn read_str(input: String) -> Option<crate::types::MalValue>  {
        let tokens = Reader::tokenize(input);
        
        let mut reader = Reader::new(tokens);

        return reader.read_form();
    }

    fn tokenize(input: String) -> Vec<String> {
        let mut tokens = Vec::<String>::new();

        let mut chars = input.chars().peekable();
        loop {
            let char = chars.next();

            if char.is_none() {
                break
            }

            let c = char.unwrap();
            
            let mut token = String::from(c);

            match c {
                // Skip white-space
                ' '|'\t'|',' => continue,
                '~' => {
                    let next = chars.peek();
                    if next == Some(&'@') {
                        tokens.push(token.to_string());
                    }
                    tokens.push(token.to_string())
                },
                '['|']'|'{'|'}'|'('|')'|'\''|'`'|'^'|'@' => {
                    tokens.push(token)
                },
                '"' => {
                    // String consume until we hit another " without a \ infront
                    while chars.peek().is_some() {
                        token += &chars.next().unwrap().to_string();
                    }
                    tokens.push(token);
                },
                ';' => {
                    // Comment, ignore the rest of the input
                    break;
                },
                _ => {
                    while chars.peek().is_some() {
                        token += &chars.next().unwrap().to_string();
                    }
                    tokens.push(token);
                }
            }
        }

        return tokens;
    }

    pub fn read_form(&mut self) -> Option<crate::types::MalValue> {
        let token = self.peek();
        match token {
            None => return None,
            Some(t) => {
                if t.starts_with('(') {
                    return self.read_list();
                }
                return self.read_atom();
            }
        }
    }

    pub fn read_list(&mut self) -> Option<crate::types::MalValue> {
        let mut token = self.next(); // Consume the '('
        assert_eq!(token, Some(String::from('(')));

        let mut tokens = Vec::<crate::types::MalValue>::new();

        loop {
            token = self.peek();
            match token {
                None => break,
                Some(t) => {
                    if t == String::from(")") {
                        break;
                    }
                    if let element = self.read_form() {
                        tokens.push(element.unwrap());
                    }
                }
            }
        }

        return Some(crate::types::MalValue::MalList(tokens));
    }

    pub fn read_atom(&mut self) -> Option<crate::types::MalValue> {
        let token = self.next().unwrap();

        return Some(crate::types::MalValue::MalString(token));
    }
}