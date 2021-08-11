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

    pub fn read_str(input: String) -> Result<Option<crate::types::MalValue>, crate::types::MalError>  {
        let tokens = Reader::tokenize(input)?;
        
        let mut reader = Reader::new(tokens);

        let token = reader.read_form()?;
        return Ok(token);
    }

    fn tokenize(input: String) -> Result<Vec<String>, crate::types::MalError> {
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
                    let mut balanced_string = false;
                    let mut current = c;
                    while let Some(next) = chars.peek() {
                        //end of the string
                        if *next == '"' && current != '\"'  {
                            balanced_string = true;
                            // consume the closing double quote
                            token += &chars.next().unwrap().to_string();
                            break;
                        }
                        current = *next;
                        token += &chars.next().unwrap().to_string();
                    }

                    if !balanced_string {
                        return Err(crate::types::MalError::ParseError(String::from("unbalanced '\"'")));
                    }

                    tokens.push(token);
                },
                ';' => {
                    // Comment, ignore the rest of the input
                    break;
                },
                _ =>  { 
                    while let Some(next_char) = chars.next_if(|&c| Reader::is_symbol_char(c)) {
                        token += &next_char.to_string();
                    }
                    tokens.push(token);
                }
            }
        }
        return Ok(tokens);
    }

    fn is_symbol_char(c: char) -> bool {
        return match c {
            '~'|'['|']'|'{'|'}'|'('|')'|'\''|'`'|'^'|'@'|'"'|';'|' '|','|'\t' => false,
            _ => true
        };
    }

    pub fn read_form(&mut self) -> Result<Option<crate::types::MalValue>, crate::types::MalError>{
        let token = self.peek();
        match token {
            None => return Ok(None),
            Some(t) => {
                if t.starts_with('(') {
                    return self.read_list();
                }
                return self.read_atom();
            }
        }
    }

    pub fn read_list(&mut self) -> Result<Option<crate::types::MalValue>, crate::types::MalError>{
        let mut token = self.next(); // Consume the '('
        assert_eq!(token, Some(String::from('(')));

        let mut tokens = Vec::<crate::types::MalValue>::new();

        let mut balanced_list = false;
        loop {
            token = self.peek();
            match token {
                None => break,
                Some(t) => {
                    if t == String::from(")") {
                        balanced_list = true;
                        break;
                    }

                    if let Some(element) = self.read_form()? {
                        tokens.push(element);
                    }
                }
            }
        }
        
        if !balanced_list {
            return Err(crate::types::MalError::ParseError(String::from("unbalanced '('")));
        }

        return Ok(Some(crate::types::MalValue::MalList(tokens)));
    }

    pub fn read_atom(&mut self) -> Result<Option<crate::types::MalValue>, crate::types::MalError> {
        let token = self.next().unwrap();

        return Ok(Some(crate::types::MalValue::MalString(token)));
    }
}