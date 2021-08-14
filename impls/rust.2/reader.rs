use super::types::{MalValue, MalError, bool};

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

    pub fn read_str(input: String) -> Result<Option<MalValue>, MalError>  {
        let tokens = Reader::tokenize(input)?;
        
        let mut reader = Reader::new(tokens);

        let token = reader.read_form()?;
        return Ok(token);
    }

    fn tokenize(input: String) -> Result<Vec<String>, MalError> {
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
                        token += &next.unwrap().to_string();
                        tokens.push(token.to_string());
                    } else {
                        tokens.push(token.to_string())
                    }
                },
                '['|']'|'{'|'}'|'('|')'|'\''|'`'|'^'|'@' => {
                    tokens.push(token)
                },
                // Start of a string
                '"' => {

                    let mut balanced_string = false;
                    let mut is_escape = false;
                    while let Some(next) = chars.peek() {
                        //println!("Char: {}, is_escape: {}", next, is_escape);
                        match next {
                            '"' => {
                                if is_escape {
                                    token += "\"";
                                    //chars.next();
                                    is_escape = false;
                                } else {
                                    balanced_string = true;
                                    token += &next.to_string();
                                    chars.next();
                                    break;
                                }
                            },
                            '\\' => {
                                if is_escape {
                                    is_escape = false;
                                    token += "\\";
                                } else {
                                    is_escape = true;
                                }
                                //chars.next();
                            },
                            'n' => {
                                if is_escape {
                                    token += "\n";
                                    is_escape = false;
                                }
                                else {
                                    token += "n";
                                }
                            }
                            _ => {
                                token += &next.to_string();
                            }
                        }
                        chars.next();
                    }

                    if !balanced_string {
                        return Err(MalError::ParseError(String::from("unbalanced '\"'")));
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

    pub fn read_form(&mut self) -> Result<Option<MalValue>, MalError>{
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
                } else if t.starts_with('\'') {
                    self.next();
                    let mut quote = Vec::<MalValue>::new();
                    quote.push(MalValue::MalSymbol(String::from("quote")));
                    if let Ok(Some(quoted_form)) = self.read_form() {
                        quote.push(quoted_form);
                    }

                    return Ok(Some(MalValue::MalList(quote)));
                } else if t.starts_with('`') {
                    self.next();
                    let mut quote = Vec::<MalValue>::new();
                    quote.push(MalValue::MalSymbol(String::from("quasiquote")));
                    if let Ok(Some(quoted_form)) = self.read_form() {
                        quote.push(quoted_form);
                    }

                    return Ok(Some(MalValue::MalList(quote)));
                } else if t.starts_with("~@") {
                    self.next();
                    self.next();
                    let mut quote = Vec::<MalValue>::new();
                    quote.push(MalValue::MalSymbol(String::from("splice-unquote")));
                    if let Ok(Some(quoted_form)) = self.read_form() {
                        quote.push(quoted_form);
                    }

                    return Ok(Some(MalValue::MalList(quote)));
                } else if t.starts_with('~') {
                    self.next();
                    let mut quote = Vec::<MalValue>::new();
                    quote.push(MalValue::MalSymbol(String::from("unquote")));
                    if let Ok(Some(quoted_form)) = self.read_form() {
                        quote.push(quoted_form);
                    }

                    return Ok(Some(MalValue::MalList(quote)));
                } else if t.starts_with('@') {
                    self.next();
                    let mut quote = Vec::<MalValue>::new();
                    quote.push(MalValue::MalSymbol(String::from("deref")));
                    if let Ok(Some(quoted_form)) = self.read_form() {
                        quote.push(quoted_form);
                    }

                    return Ok(Some(MalValue::MalList(quote)));
                }

                return self.read_atom();
            }
        }
    }

    fn read_hashmap(&mut self) -> Result<Option<MalValue>, MalError>{
        let token = self.next(); // Consume the '{'
        assert_eq!(token, Some(String::from('{')));

        let mut keys = Vec::<MalValue>::new();
        let mut values = Vec::<MalValue>::new();
        let mut index = 0;

        while let Some(token) = self.peek() {
            if token == "}" {
                self.next();
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
            return Err(MalError::ParseError(String::from("unbalanced keys and values")));
        }

        assert_eq!(keys.len(), values.len());

        return Ok(Some(MalValue::MalHashmap(keys, values)))
    }

    fn read_vector(&mut self) -> Result<Option<MalValue>, MalError>{
        let mut token = self.next(); // Consume the '['
        assert_eq!(token, Some(String::from('[')));

        let mut tokens = Vec::<MalValue>::new();

        let mut balanced_list = false;
        loop {
            token = self.peek();
            match token {
                None => break,
                Some(t) => {
                    if t == String::from("]") {
                        self.next();
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
            return Err(MalError::ParseError(String::from("unbalanced '['")));
        }

        return Ok(Some(MalValue::MalVector(tokens)));
    }

    pub fn read_list(&mut self) -> Result<Option<MalValue>, MalError>{
        let mut token = self.next(); // Consume the '('
        assert_eq!(token, Some(String::from('(')));

        let mut tokens = Vec::<MalValue>::new();

        let mut balanced_list = false;
        loop {
            token = self.peek();
            match token {
                None => break,
                Some(t) => {
                    if t == String::from(")") {
                        balanced_list = true;
                        self.next();
                        break;
                    }

                    if let Some(element) = self.read_form()? {
                        tokens.push(element);
                    }
                }
            }
        }
        
        if !balanced_list {
            return Err(MalError::ParseError(String::from("unbalanced '('")));
        }

        return Ok(Some(MalValue::MalList(tokens)));
    }

    pub fn read_atom(&mut self) -> Result<Option<MalValue>, MalError> {
        let token = self.next().unwrap();

        if token == "nil" {
            return Ok(Some(MalValue::MalNil));
        } else if token == "false" {
            return Ok(Some(bool(false)));
        } else if token == "true" {
            return Ok(Some(bool(true)));
        } else if token.starts_with('"') {
            return Ok(Some(MalValue::MalString(token)));
        } else if token.starts_with(':') {
            return Ok(Some(MalValue::MalKeyword(
                token.strip_prefix(':').unwrap_or(&token).to_string()
            )));
        } else if let Ok(int) = token.parse::<i32>() {
            return Ok(Some(MalValue::MalInteger(int)));
        }

        return Ok(Some(MalValue::MalSymbol(token)));
    }
}