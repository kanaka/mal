use std::rc::Rc;
use std::collections;
use std::fmt;
use super::printer::escape_str;

#[deriving(Clone)]
pub enum MalType {
    Nil,
    True,
    False,
    Int(int),
    Strn(String),
    Sym(String),
    List(Vec<MalVal>),
    Vector(Vec<MalVal>),
    HashMap(collections::HashMap<String, MalVal>),
}

pub type MalVal = Rc<MalType>;

impl MalType {
    pub fn pr_str(&self, print_readably: bool) -> String {
        let _r = print_readably;
        let mut res = String::new();
        match *self {
            Nil => res.push_str("nil"),
            True => res.push_str("true"),
            False => res.push_str("false"),
            Int(v) => res.push_str(v.to_string().as_slice()),
            Sym(ref v) => res.push_str((*v).as_slice()),
            Strn(ref v) => {
                if print_readably {
                    res.push_str(escape_str((*v).as_slice()).as_slice())
                } else {
                    res.push_str(v.as_slice())
                }
            }
            List(ref v) => {
                let mut first = true;
                res.push_str("(");
                for item in v.iter() {
                    if first { first = false; } else { res.push_str(" "); }
                    res.push_str(item.pr_str(_r).as_slice());
                }
                res.push_str(")")
            }
/*
*/
            /*
            Vector(ref v) => {
                let mut first = true;
                write!(f, "[");
                for item in v.iter() {
                    if first { first = false; } else { write!(f, " ") }
                    item.fmt(f);
                }
                write!(f, "]");
            }
            Hash_Map(ref v) => {
                let mut first = true;
                write!(f, "{}", "{");
                for (key, value) in v.iter() {
                    if first { first = false; } else { write!(f, " ") }
                    write!(f, "\"{}\"", *key);
                    write!(f, " ");
                    value.fmt(f);
                }
                write!(f, "{}", "}");
            }

//            Atom(ref v) => v.fmt(f),
            */
            _ => { res.push_str("#<unknown type>") }
        };
        res
    }
}

impl fmt::Show for MalType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.pr_str(true))
    }
}
