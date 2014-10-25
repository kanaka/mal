//#![feature(phase)]
//#[phase(plugin)]
//extern crate regex_macros;
//extern crate regex;

extern crate pcre;

use std::rc::Rc;
use types::{MalVal,Nil,True,False,Int,Strn,Sym,List};
use self::pcre::Pcre;
use super::printer::unescape_str;

#[deriving(Show, Clone)]
struct Reader {
    tokens   : Vec<String>,
    position : uint,
}

impl Reader {
    fn next(&mut self) -> Option<String> {
        if self.position < self.tokens.len() {
            self.position += 1;
            Some(self.tokens[self.position-1].to_string())
        } else {
            None
        }
    }
    fn peek(&self) -> Option<String> {
        if self.position < self.tokens.len() {
            Some(self.tokens[self.position].to_string())
        } else {
            None
        }
    }
}

fn tokenize(str :String) -> Vec<String> {
    let mut results = vec![];

    let re = match Pcre::compile(r###"[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"|;.*|[^\s\[\]{}('"`,;)]*)"###) {
        Err(_) => { fail!("failed to compile regex") },
        Ok(re) => re
    };

    let mut it = re.matches(str.as_slice());
    loop {
        let opt_m = it.next();
        if opt_m.is_none() { break; }
        let m = opt_m.unwrap();
        if m.group(1) == "" { break; }

        results.push((*m.group(1)).to_string());
    }
    results
}

fn read_atom(rdr : &mut Reader) -> Result<MalVal,String> {
    let otoken = rdr.next();
    //println!("read_atom: {}", otoken);
    if otoken.is_none() { return Err("read_atom underflow".to_string()); }
    let stoken = otoken.unwrap();
    let token = stoken.as_slice();
    if regex!(r"^-?[0-9]+$").is_match(token) {
        let num : Option<int> = from_str(token);
        Ok(Rc::new(Int(num.unwrap())))
    } else if regex!(r#"^".*"$"#).is_match(token) {
        let new_str = token.slice(1,token.len()-1);
        Ok(Rc::new(Strn(unescape_str(new_str))))
    } else if token == "nil" {
        Ok(Rc::new(Nil))
    } else if token == "true" {
        Ok(Rc::new(True))
    } else if token == "false" {
        Ok(Rc::new(False))
    } else {
        Ok(Rc::new(Sym(String::from_str(token))))
    }
}

fn read_list(rdr : &mut Reader) -> Result<MalVal,String> {
    let otoken = rdr.next();
    if otoken.is_none() { return Err("read_atom underflow".to_string()); }
    let stoken = otoken.unwrap();
    let token = stoken.as_slice();
    if token != "(" { return Err("expected '('".to_string()); }

    let mut ast_vec : Vec<MalVal> = vec![];
    loop {
        let otoken = rdr.peek();
        if otoken.is_none() { return Err("expected ')', got EOF".to_string()); }
        let stoken = otoken.unwrap();
        let token = stoken.as_slice();
        if token == ")" { break; }

        match read_form(rdr) {
            Ok(mv) => ast_vec.push(mv),
            Err(e) => return Err(e),
        }
    }
    rdr.next();

    //ast_vec.push(Rc::new(Nil));
    Ok(Rc::new(List(ast_vec)))
}

fn read_form(rdr : &mut Reader) -> Result<MalVal,String> {
    let otoken = rdr.peek();
    //println!("read_form: {}", otoken);
    let stoken = otoken.unwrap();
    let token = stoken.as_slice();
    match token {
        ")" => Err("unexected ')'".to_string()),
        "(" => read_list(rdr),
        _   => read_atom(rdr)
    }
}

pub fn read_str(str :String) -> Result<MalVal,String> {
    let tokens = tokenize(str);
    if tokens.len() == 0 {
        return Err("<empty line>".to_string());
    }
    //println!("tokens: {}", tokens);
    let rdr = &mut Reader{tokens: tokens, position: 0};
    read_form(rdr)
}
