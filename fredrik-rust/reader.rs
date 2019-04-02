use super::types::{MalType, Result};
use lazy_static::lazy_static;
use regex::Regex;
use std::error::Error;
use std::fmt;

#[derive(Debug)]
struct ParseError {
    err: String,
}

impl ParseError {
    fn new(err: &str) -> Result {
        Err(Box::new(ParseError {
            err: err.to_owned(),
        }))
    }
}

impl Error for ParseError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        None
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Parse error: {}", self.err)
    }
}

#[derive(Debug)]
struct Reader {
    tokens: Vec<String>,
    pos: usize,
}

impl Reader {
    pub fn new(tokens: Vec<String>) -> Self {
        Reader {
            tokens: tokens,
            pos: 0,
        }
    }

    pub fn has_next(&self) -> bool {
        self.pos < self.tokens.len()
    }

    pub fn next(&mut self) -> &str {
        let t = &self.tokens[self.pos];
        self.pos += 1;
        t
    }

    pub fn peek(&self) -> Option<&str> {
        if self.pos < self.tokens.len() {
            Some(&self.tokens[self.pos])
        } else {
            None
        }
    }
}

pub fn read_str(s: &str) -> Result {
    let tokens = tokenize(s);

    let mut rdr = Reader::new(tokens);

    read_form(&mut rdr)
}

fn read_form(rdr: &mut Reader) -> Result {
    match rdr.peek() {
        None => ParseError::new("Unexpected EOF"),
        Some("(") => read_list(rdr, ")"),
        Some("[") => read_list(rdr, "]"),
        Some("{") => read_list(rdr, "}"),
        _ => read_atom(rdr),
    }
}

fn read_list(rdr: &mut Reader, closer: &str) -> Result {
    let mut res = Vec::new();
    // skip (
    rdr.next();

    loop {
        let next = rdr.peek();
        if next.is_none() {
            return ParseError::new(&format!("EOF before {}", closer));
        }

        if next.unwrap() == closer {
            // consume the ) and then exit loop
            rdr.next();
            break;
        }

        let v = read_form(rdr)?;
        res.push(v);
    }

    if closer == ")" {
        Ok(MalType::List(res))
    } else if closer == "}" {
        Ok(MalType::HashMap(res))
    } else {
        Ok(MalType::Vector(res))
    }
}

fn read_atom(rdr: &mut Reader) -> Result {
    match rdr.next() {
        "nil" => Ok(MalType::Nil),
        "true" => Ok(MalType::Boolean(true)),
        "false" => Ok(MalType::Boolean(false)),
        "'" => parse_quote(rdr),
        "`" => parse_quasiquote(rdr),
        "~" => parse_unquote(rdr),
        "~@" => parse_splice_unquote(rdr),
        "^" => parse_meta(rdr),
        "@" => parse_deref(rdr),
        s => match s.parse::<isize>() {
            Ok(i) => Ok(MalType::Integer(i)),
            _ => {
                if &s[0..1] == "\"" {
                    if &s[s.len() - 1..] == "\"" {
                        Ok(MalType::String(s.to_string()))
                    } else {
                        ParseError::new("unbalanced \"")
                    }
                } else {
                    Ok(MalType::Symbol(s.to_string()))
                }
            }
        },
    }
}

fn parse_quote(rdr: &mut Reader) -> Result {
    if rdr.has_next() {
        let q = read_form(rdr)?;
        Ok(MalType::Quote(Box::new(q)))
    } else {
        ParseError::new("error parsing quote")
    }
}

fn parse_quasiquote(rdr: &mut Reader) -> Result {
    if rdr.has_next() {
        let q = read_form(rdr)?;
        Ok(MalType::QuasiQuote(Box::new(q)))
    } else {
        ParseError::new("error parsing quasiquote")
    }
}

fn parse_unquote(rdr: &mut Reader) -> Result {
    if rdr.has_next() {
        let q = read_form(rdr)?;
        Ok(MalType::UnQuote(Box::new(q)))
    } else {
        ParseError::new("error parsing unquote")
    }
}

fn parse_splice_unquote(rdr: &mut Reader) -> Result {
    if rdr.has_next() {
        let q = read_form(rdr)?;
        Ok(MalType::SpliceUnQuote(Box::new(q)))
    } else {
        ParseError::new("error parsing splice-unquote")
    }
}

fn parse_meta(rdr: &mut Reader) -> Result {
    if !rdr.has_next() {
        return ParseError::new("error reading first meta");
    }
    let b = read_form(rdr)?;

    if !rdr.has_next() {
        return ParseError::new("error reading second meta");
    }

    let a = read_form(rdr)?;

    Ok(MalType::WithMeta(Box::new(a), Box::new(b)))
}

fn parse_deref(rdr: &mut Reader) -> Result {
    if !rdr.has_next() {
        return ParseError::new("error reading deref");
    }

    Ok(MalType::Deref(rdr.next().to_string()))
}

fn tokenize(input: &str) -> Vec<String> {
    lazy_static! {
        static ref RE: Regex =
            Regex::new(r#"[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"?|;.*|[^\s\[\]{}('"`,;)]*)"#)
                .unwrap();
    }

    let mut res = Vec::new();
    for cap in RE.captures_iter(input) {
        // println!("cap: {:?}", cap);
        let m = cap.get(1).map_or("", |m| m.as_str());
        if !m.is_empty() {
            res.push(m.to_string());
        }
    }

    res
}

// named!(escaped_string<CompleteStr, String>,
//         delimited!(
//             char!('"'),
//             map!(
//                 many0!(
//                     alt!(
//                         tag!("\\\"") => { |_| '"' } |
//                         none_of!("\"")
//                     )
//                 ),
//                 |v| v.iter().collect::<String>()
//                 ),
//             char!('"')
//             )
//         );

// named!(p_string<CompleteStr, MalType>, do_parse! (
//         s: escaped_string >>
//         (MalType::String(format!(r#""{}""#, s)))
//         ));

// named!(p_symbol<CompleteStr, MalType>, do_parse! (
//         s:
//         ));

#[cfg(test)]
mod tests {
    use super::*;

    // fn string_match(m: &MalType, b: &str) {
    //     match m {
    //         MalType::String(a) => {
    //             println!("{}", a);
    //             assert_eq!(a, b);
    //         }
    //         _ => assert!(false),
    //     }
    // }

    // #[test]
    // fn reader_parse_string() {
    //     let r1 = p_string(CompleteStr(r#""fredrik""#));
    //     string_match(&r1.unwrap().1, "\"fredrik\"");

    //     let r2 = p_string(CompleteStr(r#""\"fredrik\"jansson""#));
    //     string_match(&r2.unwrap().1, r#""\"fredrik\"jansson""#);

    //     string_match(
    //         &p_string(CompleteStr(r#""fredrik jansson""#)).unwrap().1,
    //         r#""fredrik jansson""#,
    //     );
    // }

    #[test]
    fn test_read_str() {
        match read_str("sym") {
            Ok(MalType::Symbol(s)) => assert_eq!(s, "sym"),
            e => panic!("Symbol parsing failed: {:?}", e),
        }

        match read_str("let*") {
            Ok(MalType::Symbol(s)) => assert_eq!(s, "let*"),
            e => panic!("Symbol parsing failed: {:?}", e),
        }

        match read_str(r#""str""#) {
            Ok(MalType::String(s)) => assert_eq!(s, "\"str\""),
            e => panic!("String parsing failed: {:?}", e),
        }

        match read_str(r#""str\n\"\\""#) {
            Ok(MalType::String(s)) => assert_eq!(s, r#""str\n\"\\""#),
            e => panic!("String parsing failed: {:?}", e),
        }

        match read_str(r#""fredrik jansson""#) {
            Ok(MalType::String(s)) => assert_eq!(s, r#""fredrik jansson""#),
            e => panic!("String parsing failed: {:?}", e),
        }

        match read_str("nil") {
            Ok(MalType::Nil) => (),
            e => panic!("Nil parsing failed: {:?}", e),
        }
    }
}
