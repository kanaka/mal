pub struct Reader {
    position: usize,
    tokens: Vec<String>
}

impl Reader {
    fn new(tokens: Vec<String>) -> Reader {
        return Reader{
            position: 0,
            tokens
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
                        // Previous character was an escape, add a literal
                        // of the current character
                        if current == '\\' {
                            match next {
                                // Escaped \ or "
                                '\\'|'"' => token += &next.to_string(),
                                // Line break
                                'n' => token += "\n",
                                // Tab
                                't' => token += "\t",
                                _ => {
                                    return Err(crate::types::MalError::ParseError(String::from("Invalid escape")));
                                }
                            }
                        } else {
                            match *next {
                                '\\' => {
                                    // Ignore the character for now
                                    //chars.next();
                                },
                                '"' => {
                                    balanced_string = true;
                                    token += &*next.to_string();
                                },
                                _ => {
                                    token += &*next.to_string();
                                }
                            }
                        }
                        current = *next;
                        chars.next();
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
        return !matches!(c, '~'|'['|']'|'{'|'}'|'('|')'|'\''|'`'|'^'|'@'|'"'|';'|' '|','|'\t');
    }

    pub fn read_form(&mut self) -> Result<Option<crate::types::MalValue>, crate::types::MalError>{
        let token = self.peek();
        match token {
            None => return Ok(None),
            Some(t) => {
                if t.starts_with('(') {
                    return self.read_list();
                } else if t.starts_with('[') {
                    return self.read_vector();
                } else if t.starts_with('{') {
                    return self.read_hashmap();
                }

                return self.read_atom();
            }
        }
    }

    fn read_hashmap(&mut self) -> Result<Option<crate::types::MalValue>, crate::types::MalError>{
        let token = self.next(); // Consume the '{'
        assert_eq!(token, Some(String::from('{')));

        let mut keys = Vec::<crate::types::MalValue>::new();
        let mut values = Vec::<crate::types::MalValue>::new();
        let mut index = 0;

        while let Some(token) = self.peek() {
            if token == "}" {
                break;
            }
            if index % 2 == 0 {
                keys.push(self.read_form()?.unwrap());
            } else {
                values.push(self.read_form()?.unwrap());
            }
            index += 1;
        }

        if keys.len() != values.len() {
            return Err(crate::types::MalError::ParseError(String::from("unbalanced keys and values")));
        }

        assert_eq!(keys.len(), values.len());

        return Ok(Some(crate::types::MalValue::MalHashmap(keys, values)))
    }

    fn read_vector(&mut self) -> Result<Option<crate::types::MalValue>, crate::types::MalError>{
        let mut token = self.next(); // Consume the '['
        assert_eq!(token, Some(String::from('[')));

        let mut tokens = Vec::<crate::types::MalValue>::new();

        let mut balanced_list = false;
        loop {
            token = self.peek();
            match token {
                None => break,
                Some(t) => {
                    if t == String::from("]") {
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
            return Err(crate::types::MalError::ParseError(String::from("unbalanced '['")));
        }

        return Ok(Some(crate::types::MalValue::MalVector(tokens)));
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

        if token.starts_with('"') {
            return Ok(Some(crate::types::MalValue::MalString(token)));
        } else if token.starts_with(':') {
            return Ok(Some(crate::types::MalValue::MalKeyword(
                token.strip_prefix(':').unwrap_or(&token).to_string()
            )));
        } else if let Ok(int) = token.parse::<i32>() {
            return Ok(Some(crate::types::MalValue::MalInteger(int)));
        }

        return Ok(Some(crate::types::MalValue::MalSymbol(token)));
    }
}