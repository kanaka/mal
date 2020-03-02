use crate::types::MalVal;
use crate::types::MalVal::{Atom, Bool, Func, Hash, Int, List, MalFunc, Nil, Str, Sym, Vector};

fn escape_str(s: &str) -> String {
    s.chars()
        .map(|c| match c {
            '"' => "\\\"".to_string(),
            '\n' => "\\n".to_string(),
            '\\' => "\\\\".to_string(),
            _ => c.to_string(),
        })
        .collect::<Vec<String>>()
        .join("")
}

impl MalVal {
    pub fn pr_str(&self, print_readably: bool) -> String {
        match self {
            Nil => String::from("nil"),
            Bool(true) => String::from("true"),
            Bool(false) => String::from("false"),
            Int(i) => format!("{}", i),
            //Float(f)    => format!("{}", f),
            Str(s) => {
                if s.starts_with("\u{29e}") {
                    format!(":{}", &s[2..])
                } else if print_readably {
                    format!("\"{}\"", escape_str(s))
                } else {
                    s.clone()
                }
            }
            Sym(s) => s.clone(),
            List(l, _) => pr_seq(&**l, print_readably, "(", ")", " "),
            Vector(l, _) => pr_seq(&**l, print_readably, "[", "]", " "),
            Hash(hm, _) => {
                let l: Vec<MalVal> = hm
                    .iter()
                    .flat_map(|(k, v)| vec![Str(k.to_string()), v.clone()])
                    .collect();
                pr_seq(&l, print_readably, "{", "}", " ")
            }
            Func(f, _) => format!("#<fn {:?}>", f),
            MalFunc {
                ast: a, params: p, ..
            } => format!("(fn* {} {})", p.pr_str(true), a.pr_str(true)),
            Atom(a) => format!("(atom {})", a.borrow().pr_str(true)),
        }
    }
}

pub fn pr_seq(
    seq: &Vec<MalVal>,
    print_readably: bool,
    start: &str,
    end: &str,
    join: &str,
) -> String {
    let strs: Vec<String> = seq.iter().map(|x| x.pr_str(print_readably)).collect();
    format!("{}{}{}", start, strs.join(join), end)
}
