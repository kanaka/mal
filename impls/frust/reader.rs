use super::types::MalType;
use super::MalError;

#[derive(Debug)]
pub struct Reader {
    pos: usize,
    tokens: Vec<String>,
}

impl Reader {
    fn next(&mut self) -> String {
        self.pos += 1;
        self.tokens[self.pos - 1].clone()
    }

    fn peek(&self) -> Option<&str> {
        if self.pos >= self.tokens.len() {
            None
        } else {
            Some(&self.tokens[self.pos])
        }
    }
}

pub(crate) fn read_str(s: String) -> super::MalResult {
    let tokens = tokenize(s);
    let mut reader = Reader { pos: 0, tokens };
    read_form(&mut reader, None)
}

fn read_form(r: &mut Reader, meta: Option<Box<MalType>>) -> super::MalResult {
    match r.peek() {
        Some(s) if s == "(" => read_list(r, ListType::List, meta),
        Some(s) if s == "[" => read_list(r, ListType::Vector, meta),
        Some(s) if s == "{" => read_list(r, ListType::Map, meta),
        Some(_) => read_atom(r),
        None => Ok(MalType::Nil),
    }
}

enum ListType {
    List,
    Vector,
    Map,
}

fn read_list(r: &mut Reader, t: ListType, meta: Option<Box<MalType>>) -> super::MalResult {
    // Skip first parenthesis
    let _ = r.next();

    let closing_paren = match t {
        ListType::List => ")",
        ListType::Vector => "]",
        ListType::Map => "}",
    };

    let mut v = Vec::new();
    loop {
        match r.peek() {
            None => return Err(MalError::MismatchedParen),
            Some(v) if v == closing_paren => break,
            _ => (),
        };

        let val: MalType = read_form(r, None)?;
        v.push(Box::new(val));
    }
    // Skip last parenthesis
    let _ = r.next();

    match t {
        ListType::List => Ok(MalType::List(v, meta)),
        ListType::Vector => Ok(MalType::Vector(v, meta)),
        ListType::Map => {
            if v.len() % 2 != 0 {
                Err(MalError::OddNumParamsInMap)
            } else {
                let mut hm = std::collections::HashMap::new();
                for kv in v.chunks(2) {
                    if let MalType::String(ref s) = *kv[0] {
                        hm.insert(s.clone(), (*kv[1]).clone());
                    } else {
                        return Err(MalError::NonStringKey);
                    }
                }
                Ok(MalType::HashMap(hm, meta))
            }
        }
    }
}

fn read_atom(r: &mut Reader) -> super::MalResult {
    let token = r.next();

    // Is this a number?
    if let Ok(num) = token.parse::<f32>() {
        return Ok(MalType::Number(num));
    }

    match token {
        t if t.starts_with(":") => {
            // Skip the colon
            let rest: String = t.chars().skip(1).collect();
            Ok(MalType::String(format!(
                "{}{}",
                super::types::KEYWORD_ESC,
                rest
            )))
        }
        t if t.starts_with("\"") => read_string(t),
        t if t == "'" => {
            let q = read_form(r, None)?;
            Ok(MalType::Quote(Box::new(q)))
        }
        t if t == "`" => {
            let q = read_form(r, None)?;
            Ok(MalType::QuasiQuote(Box::new(q)))
        }
        t if t == "~@" => {
            let q = read_form(r, None)?;
            Ok(MalType::SpliceUnquote(Box::new(q)))
        }
        t if t == "~" => {
            let q = read_form(r, None)?;
            Ok(MalType::Unquote(Box::new(q)))
        }
        t if t == "@" => {
            let q = read_form(r, None)?;
            Ok(MalType::Deref(Box::new(q)))
        }
        t if t == "^" => {
            let meta = read_form(r, None)?;
            read_form(r, Some(Box::new(meta)))
        }
        _ => Ok(MalType::Symbol(token)),
    }
}

fn read_string(s: String) -> super::MalResult {
    // Skip "
    let mut chars = s.chars().skip(1);
    let mut res = String::new();
    loop {
        let c = chars.next();
        match c {
            None => {
                return Err(MalError::StringMismatchedDoubleQuote);
            }
            // Break when we find the quote
            Some(c) if c == '\"' => {
                break;
            }
            // Handle escapes
            Some(c) if c == '\\' => match chars.next() {
                Some('\"') => res.push('\"'),
                Some('\\') => res.push('\\'),
                Some('n') => res.push('\n'),
                _ => {
                    return Err(MalError::StringEscape);
                }
            },
            Some(c) => res.push(c),
        }
    }
    Ok(MalType::String(res))
}

fn tokenize(s: String) -> Vec<String> {
    lazy_static::lazy_static! {
    static ref RE: regex::Regex = regex::Regex::new(r#"[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"?|;.*|[^\s\[\]{}('"`,;)]*)"#).unwrap();
                };

    RE.captures_iter(&s).map(|m| m[1].to_owned()).collect()
}

#[cfg(test)]
mod tests {
    #[test]
    fn tokenize() {
        let tokens = super::tokenize("(+ 11 22   )".to_owned());

        assert_eq!(tokens.len(), 5);

        assert!(tokens[0] == "(");
        assert!(tokens[1] == "+");
        assert!(tokens[2] == "11");
        assert!(tokens[3] == "22");
        assert!(tokens[4] == ")");
    }
}
