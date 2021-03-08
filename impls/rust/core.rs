use std::fs::File;
use std::io::Read;
use std::rc::Rc;
use std::sync::Mutex;
use std::time::{SystemTime, UNIX_EPOCH};

extern crate rustyline;
use rustyline::error::ReadlineError;
use rustyline::Editor;

use crate::printer::pr_seq;
use crate::reader::read_str;
use crate::types::MalErr::ErrMalVal;
use crate::types::MalVal::{Atom, Bool, Func, Hash, Int, List, MalFunc, Nil, Str, Sym, Vector};
use crate::types::{MalArgs, MalRet, MalVal, _assoc, _dissoc, atom, error, func, hash_map};

macro_rules! fn_t_int_int {
    ($ret:ident, $fn:expr) => {{
        |a: MalArgs| match (a[0].clone(), a[1].clone()) {
            (Int(a0), Int(a1)) => Ok($ret($fn(a0, a1))),
            _ => error("expecting (int,int) args"),
        }
    }};
}

macro_rules! fn_is_type {
  ($($ps:pat),*) => {{
    |a:MalArgs| { Ok(Bool(match a[0] { $($ps => true,)* _ => false})) }
  }};
  ($p:pat if $e:expr) => {{
    |a:MalArgs| { Ok(Bool(match a[0] { $p if $e => true, _ => false})) }
  }};
  ($p:pat if $e:expr,$($ps:pat),*) => {{
    |a:MalArgs| { Ok(Bool(match a[0] { $p if $e => true, $($ps => true,)* _ => false})) }
  }};
}

macro_rules! fn_str {
    ($fn:expr) => {{
        |a: MalArgs| match a[0].clone() {
            Str(a0) => $fn(a0),
            _ => error("expecting (str) arg"),
        }
    }};
}

fn symbol(a: MalArgs) -> MalRet {
    match a[0] {
        Str(ref s) => Ok(Sym(s.to_string())),
        _ => error("illegal symbol call"),
    }
}

fn readline(a: MalArgs) -> MalRet {
    lazy_static! {
        static ref RL: Mutex<Editor<()>> = Mutex::new(Editor::<()>::new());
    }
    //let mut rl = Editor::<()>::new();

    match a[0] {
        Str(ref p) => {
            //match rl.readline(p) {
            match RL.lock().unwrap().readline(p) {
                Ok(mut line) => {
                    // Remove any trailing \n or \r\n
                    if line.ends_with('\n') {
                        line.pop();
                        if line.ends_with('\r') {
                            line.pop();
                        }
                    }
                    Ok(Str(line))
                }
                Err(ReadlineError::Eof) => Ok(Nil),
                Err(e) => error(&format!("{:?}", e)),
            }
        }
        _ => error("readline: prompt is not Str"),
    }
}

fn slurp(f: String) -> MalRet {
    let mut s = String::new();
    match File::open(f).and_then(|mut f| f.read_to_string(&mut s)) {
        Ok(_) => Ok(Str(s)),
        Err(e) => error(&e.to_string()),
    }
}

fn time_ms(_a: MalArgs) -> MalRet {
    let ms_e = match SystemTime::now().duration_since(UNIX_EPOCH) {
        Ok(d) => d,
        Err(e) => return error(&format!("{:?}", e)),
    };
    Ok(Int(
        ms_e.as_secs() as i64 * 1000 + ms_e.subsec_nanos() as i64 / 1_000_000
    ))
}

fn get(a: MalArgs) -> MalRet {
    match (a[0].clone(), a[1].clone()) {
        (Nil, _) => Ok(Nil),
        (Hash(ref hm, _), Str(ref s)) => match hm.get(s) {
            Some(mv) => Ok(mv.clone()),
            None => Ok(Nil),
        },
        _ => error("illegal get args"),
    }
}

fn assoc(a: MalArgs) -> MalRet {
    match a[0] {
        Hash(ref hm, _) => _assoc((**hm).clone(), a[1..].to_vec()),
        _ => error("assoc on non-Hash Map"),
    }
}

fn dissoc(a: MalArgs) -> MalRet {
    match a[0] {
        Hash(ref hm, _) => _dissoc((**hm).clone(), a[1..].to_vec()),
        _ => error("dissoc on non-Hash Map"),
    }
}

fn contains_q(a: MalArgs) -> MalRet {
    match (a[0].clone(), a[1].clone()) {
        (Hash(ref hm, _), Str(ref s)) => Ok(Bool(hm.contains_key(s))),
        _ => error("illegal get args"),
    }
}

fn keys(a: MalArgs) -> MalRet {
    match a[0] {
        Hash(ref hm, _) => Ok(list!(hm.keys().map(|k| { Str(k.to_string()) }).collect())),
        _ => error("keys requires Hash Map"),
    }
}

fn vals(a: MalArgs) -> MalRet {
    match a[0] {
        Hash(ref hm, _) => Ok(list!(hm.values().map(|v| { v.clone() }).collect())),
        _ => error("keys requires Hash Map"),
    }
}

fn vec(a: MalArgs) -> MalRet {
    match a[0] {
        List(ref v, _) | Vector(ref v, _) => Ok(vector!(v.to_vec())),
        _ => error("non-seq passed to vec"),
    }
}

fn cons(a: MalArgs) -> MalRet {
    match a[1].clone() {
        List(v, _) | Vector(v, _) => {
            let mut new_v = vec![a[0].clone()];
            new_v.extend_from_slice(&v);
            Ok(list!(new_v.to_vec()))
        }
        _ => error("cons expects seq as second arg"),
    }
}

fn concat(a: MalArgs) -> MalRet {
    let mut new_v = vec![];
    for seq in a.iter() {
        match seq {
            List(v, _) | Vector(v, _) => new_v.extend_from_slice(v),
            _ => return error("non-seq passed to concat"),
        }
    }
    Ok(list!(new_v.to_vec()))
}

fn nth(a: MalArgs) -> MalRet {
    match (a[0].clone(), a[1].clone()) {
        (List(seq, _), Int(idx)) | (Vector(seq, _), Int(idx)) => {
            if seq.len() <= idx as usize {
                return error("nth: index out of range");
            }
            Ok(seq[idx as usize].clone())
        }
        _ => error("invalid args to nth"),
    }
}

fn first(a: MalArgs) -> MalRet {
    match a[0].clone() {
        List(ref seq, _) | Vector(ref seq, _) if seq.len() == 0 => Ok(Nil),
        List(ref seq, _) | Vector(ref seq, _) => Ok(seq[0].clone()),
        Nil => Ok(Nil),
        _ => error("invalid args to first"),
    }
}

fn rest(a: MalArgs) -> MalRet {
    match a[0].clone() {
        List(ref seq, _) | Vector(ref seq, _) => {
            if seq.len() > 1 {
                Ok(list!(seq[1..].to_vec()))
            } else {
                Ok(list![])
            }
        }
        Nil => Ok(list![]),
        _ => error("invalid args to first"),
    }
}

fn apply(a: MalArgs) -> MalRet {
    match a[a.len() - 1] {
        List(ref v, _) | Vector(ref v, _) => {
            let f = &a[0];
            let mut fargs = a[1..a.len() - 1].to_vec();
            fargs.extend_from_slice(&v);
            f.apply(fargs)
        }
        _ => error("apply called with non-seq"),
    }
}

fn map(a: MalArgs) -> MalRet {
    match a[1] {
        List(ref v, _) | Vector(ref v, _) => {
            let mut res = vec![];
            for mv in v.iter() {
                res.push(a[0].apply(vec![mv.clone()])?)
            }
            Ok(list!(res))
        }
        _ => error("map called with non-seq"),
    }
}

fn conj(a: MalArgs) -> MalRet {
    match a[0] {
        List(ref v, _) => {
            let sl = a[1..]
                .iter()
                .rev()
                .map(|a| a.clone())
                .collect::<Vec<MalVal>>();
            Ok(list!([&sl[..], v].concat()))
        }
        Vector(ref v, _) => Ok(vector!([v, &a[1..]].concat())),
        _ => error("conj: called with non-seq"),
    }
}

fn seq(a: MalArgs) -> MalRet {
    match a[0] {
        List(ref v, _) | Vector(ref v, _) if v.len() == 0 => Ok(Nil),
        List(ref v, _) | Vector(ref v, _) => Ok(list!(v.to_vec())),
        Str(ref s) if s.len() == 0 => Ok(Nil),
        Str(ref s) if !a[0].keyword_q() => {
            Ok(list!(s.chars().map(|c| { Str(c.to_string()) }).collect()))
        }
        Nil => Ok(Nil),
        _ => error("seq: called with non-seq"),
    }
}

pub fn ns() -> Vec<(&'static str, MalVal)> {
    vec![
        ("=", func(|a| Ok(Bool(a[0] == a[1])))),
        ("throw", func(|a| Err(ErrMalVal(a[0].clone())))),
        ("nil?", func(fn_is_type!(Nil))),
        ("true?", func(fn_is_type!(Bool(true)))),
        ("false?", func(fn_is_type!(Bool(false)))),
        ("symbol", func(symbol)),
        ("symbol?", func(fn_is_type!(Sym(_)))),
        (
            "string?",
            func(fn_is_type!(Str(ref s) if !s.starts_with("\u{29e}"))),
        ),
        ("keyword", func(|a| a[0].keyword())),
        (
            "keyword?",
            func(fn_is_type!(Str(ref s) if s.starts_with("\u{29e}"))),
        ),
        ("number?", func(fn_is_type!(Int(_)))),
        (
            "fn?",
            func(fn_is_type!(MalFunc{is_macro,..} if !is_macro,Func(_,_))),
        ),
        (
            "macro?",
            func(fn_is_type!(MalFunc{is_macro,..} if is_macro)),
        ),
        ("pr-str", func(|a| Ok(Str(pr_seq(&a, true, "", "", " "))))),
        ("str", func(|a| Ok(Str(pr_seq(&a, false, "", "", ""))))),
        (
            "prn",
            func(|a| {
                println!("{}", pr_seq(&a, true, "", "", " "));
                Ok(Nil)
            }),
        ),
        (
            "println",
            func(|a| {
                println!("{}", pr_seq(&a, false, "", "", " "));
                Ok(Nil)
            }),
        ),
        ("read-string", func(fn_str!(|s| { read_str(s) }))),
        ("readline", func(readline)),
        ("slurp", func(fn_str!(|f| { slurp(f) }))),
        ("<", func(fn_t_int_int!(Bool, |i, j| { i < j }))),
        ("<=", func(fn_t_int_int!(Bool, |i, j| { i <= j }))),
        (">", func(fn_t_int_int!(Bool, |i, j| { i > j }))),
        (">=", func(fn_t_int_int!(Bool, |i, j| { i >= j }))),
        ("+", func(fn_t_int_int!(Int, |i, j| { i + j }))),
        ("-", func(fn_t_int_int!(Int, |i, j| { i - j }))),
        ("*", func(fn_t_int_int!(Int, |i, j| { i * j }))),
        ("/", func(fn_t_int_int!(Int, |i, j| { i / j }))),
        ("time-ms", func(time_ms)),
        ("sequential?", func(fn_is_type!(List(_, _), Vector(_, _)))),
        ("list", func(|a| Ok(list!(a)))),
        ("list?", func(fn_is_type!(List(_, _)))),
        ("vector", func(|a| Ok(vector!(a)))),
        ("vector?", func(fn_is_type!(Vector(_, _)))),
        ("hash-map", func(|a| hash_map(a))),
        ("map?", func(fn_is_type!(Hash(_, _)))),
        ("assoc", func(assoc)),
        ("dissoc", func(dissoc)),
        ("get", func(get)),
        ("contains?", func(contains_q)),
        ("keys", func(keys)),
        ("vals", func(vals)),
        ("vec", func(vec)),
        ("cons", func(cons)),
        ("concat", func(concat)),
        ("empty?", func(|a| a[0].empty_q())),
        ("nth", func(nth)),
        ("first", func(first)),
        ("rest", func(rest)),
        ("count", func(|a| a[0].count())),
        ("apply", func(apply)),
        ("map", func(map)),
        ("conj", func(conj)),
        ("seq", func(seq)),
        ("meta", func(|a| a[0].get_meta())),
        ("with-meta", func(|a| a[0].clone().with_meta(&a[1]))),
        ("atom", func(|a| Ok(atom(&a[0])))),
        ("atom?", func(fn_is_type!(Atom(_)))),
        ("deref", func(|a| a[0].deref())),
        ("reset!", func(|a| a[0].reset_bang(&a[1]))),
        ("swap!", func(|a| a[0].swap_bang(&a[1..].to_vec()))),
    ]
}
