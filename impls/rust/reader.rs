use regex::{Captures, Regex};
use std::rc::Rc;

use crate::types::MalVal::{Bool, Int, Kwd, List, Nil, Str, Sym};
use crate::types::{error, hash_map, list, vector, MalRet, MalVal};

#[derive(Debug, Clone)]
struct Reader {
    tokens: Vec<String>,
    pos: usize,
}

impl Reader {
    fn next(&mut self) -> Result<String, MalVal> {
        self.pos += 1;
        Ok(self
            .tokens
            .get(self.pos - 1)
            .ok_or_else(|| Str("underflow".to_string()))?
            .to_string())
    }
    fn peek(&self) -> Result<String, MalVal> {
        Ok(self
            .tokens
            .get(self.pos)
            .ok_or_else(|| Str("underflow".to_string()))?
            .to_string())
    }
}

thread_local! {
    static TOKENIZE_RE: Regex = Regex::new(
        r###"[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"?|;.*|[^\s\[\]{}('"`,;)]+)"###
    ).unwrap();
    static UNESCAPE_RE: Regex = Regex::new(r#"\\(.)"#).unwrap();
    static INT_RE: Regex = Regex::new(r"^-?[0-9]+$").unwrap();
    static STR_RE: Regex = Regex::new(r#""(?:\\.|[^\\"])*""#).unwrap();
}

fn tokenize(str: &str) -> Vec<String> {
    TOKENIZE_RE.with(|re| {
        let mut res = vec![];
        for cap in re.captures_iter(str) {
            if cap[1].starts_with(';') {
                continue;
            }
            res.push(String::from(&cap[1]));
        }
        res
    })
}

fn unescape_str(s: &str) -> String {
    UNESCAPE_RE.with(|re| {
        re.replace_all(s, |caps: &Captures| {
            if &caps[1] == "n" { "\n" } else { &caps[1] }.to_string()
        })
        .to_string()
    })
}

fn read_atom(rdr: &mut Reader) -> MalRet {
    let token = rdr.next()?;
    match &token[..] {
        "nil" => Ok(Nil),
        "false" => Ok(Bool(false)),
        "true" => Ok(Bool(true)),
        _ => {
            if INT_RE.with(|re| re.is_match(&token)) {
                Ok(Int(token.parse().unwrap()))
            } else if STR_RE.with(|re| re.is_match(&token)) {
                Ok(Str(unescape_str(&token[1..token.len() - 1])))
            } else if token.starts_with('\"') {
                error("expected '\"', got EOF")
            } else if let Some(keyword) = token.strip_prefix(':') {
                Ok(Kwd(String::from(keyword)))
            } else {
                Ok(Sym(token.to_string()))
            }
        }
    }
}

fn read_seq(rdr: &mut Reader, end: &str) -> Result<Vec<MalVal>, MalVal> {
    let mut seq: Vec<MalVal> = vec![];
    rdr.next()?;
    loop {
        let token = match rdr.peek() {
            Ok(t) => t,
            Err(_) => return error(&format!("expected '{}', got EOF", end)),
        };
        if token == end {
            break;
        }
        seq.push(read_form(rdr)?);
    }
    let _ = rdr.next();
    Ok(seq)
}

fn read_form(rdr: &mut Reader) -> MalRet {
    let token = rdr.peek()?;
    match &token[..] {
        "'" => {
            let _ = rdr.next();
            Ok(list!(Sym("quote".to_string()), read_form(rdr)?))
        }
        "`" => {
            let _ = rdr.next();
            Ok(list!(Sym("quasiquote".to_string()), read_form(rdr)?))
        }
        "~" => {
            let _ = rdr.next();
            Ok(list!(Sym("unquote".to_string()), read_form(rdr)?))
        }
        "~@" => {
            let _ = rdr.next();
            Ok(list!(Sym("splice-unquote".to_string()), read_form(rdr)?))
        }
        "^" => {
            let _ = rdr.next();
            let meta = read_form(rdr)?;
            Ok(list!(Sym("with-meta".to_string()), read_form(rdr)?, meta))
        }
        "@" => {
            let _ = rdr.next();
            Ok(list!(Sym("deref".to_string()), read_form(rdr)?))
        }
        ")" => error("unexpected ')'"),
        "(" => Ok(list(read_seq(rdr, ")")?)),
        "]" => error("unexpected ']'"),
        "[" => Ok(vector(read_seq(rdr, "]")?)),
        "}" => error("unexpected '}'"),
        "{" => hash_map(read_seq(rdr, "}")?.to_vec()),
        _ => read_atom(rdr),
    }
}

pub fn read_str(str: &str) -> MalRet {
    let tokens = tokenize(str);
    //println!("tokens: {:?}", tokens);
    if tokens.is_empty() {
        return error("no input");
    }
    read_form(&mut Reader { pos: 0, tokens })
}
