use std::fs::File;
use std::io::Read;
use std::rc::Rc;
use std::time::{SystemTime, UNIX_EPOCH};

use crate::printer::pr_seq;
use crate::reader::read_str;
use crate::types::MalVal::{
    Atom, Bool, Func, Hash, Int, Kwd, List, MalFunc, Nil, Str, Sym, Vector,
};
use crate::types::{
    list, FuncStruct, MalArgs, MalRet, MalVal, _assoc, error, func, hash_map, unwrap_map_key,
    vector, wrap_map_key,
};
use readline;

macro_rules! fn_t_int_int {
    ($ret:ident, $fn:expr) => {{
        |a: MalArgs| match (&a[0], &a[1]) {
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
        |a: MalArgs| match &a[0] {
            Str(a0) => $fn(&a0),
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

fn readline(p: &str) -> MalRet {
    match readline::readline(p) {
        Some(s) => Ok(Str(s)),
        None => Ok(Nil),
    }
}

fn slurp(f: &str) -> MalRet {
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
    match a[0] {
        Nil => Ok(Nil),
        Hash(ref hm, _) => match hm.get(&wrap_map_key(&a[1])?) {
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
        Hash(ref hm, _) => {
            let mut new_hm = (**hm).clone();
            for k in a[1..].iter() {
                let _ = new_hm.remove(&wrap_map_key(k)?);
            }
            Ok(Hash(Rc::new(new_hm), Rc::new(Nil)))
        }
        _ => error("dissoc on non-Hash Map"),
    }
}

fn contains_q(a: MalArgs) -> MalRet {
    match a[0] {
        Hash(ref hm, _) => Ok(Bool(hm.contains_key(&wrap_map_key(&a[1])?))),
        _ => error("illegal get args"),
    }
}

fn keys(a: MalArgs) -> MalRet {
    match a[0] {
        Hash(ref hm, _) => Ok(list(hm.keys().map(|k| unwrap_map_key(k)).collect())),
        _ => error("keys requires Hash Map"),
    }
}

fn vals(a: MalArgs) -> MalRet {
    match a[0] {
        Hash(ref hm, _) => Ok(list(hm.values().cloned().collect())),
        _ => error("vals requires Hash Map"),
    }
}

fn vec(a: MalArgs) -> MalRet {
    match a[0] {
        List(ref v, _) => Ok(Vector(v.clone(), Rc::new(Nil))),
        Vector(_, _) => Ok(a[0].clone()),
        _ => error("non-seq passed to vec"),
    }
}

fn cons(a: MalArgs) -> MalRet {
    match &a[1] {
        List(v, _) | Vector(v, _) => {
            let mut new_v = vec![a[0].clone()];
            new_v.extend_from_slice(v);
            Ok(list(new_v))
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
    Ok(list(new_v))
}

fn nth(a: MalArgs) -> MalRet {
    match (&a[0], &a[1]) {
        (List(seq, _) | Vector(seq, _), Int(idx)) => match seq.get(*idx as usize) {
            Some(result) => Ok(result.clone()),
            None => error("nth: index out of range"),
        },
        _ => error("invalid args to nth"),
    }
}

fn first(a: MalArgs) -> MalRet {
    match a[0] {
        List(ref seq, _) | Vector(ref seq, _) if seq.len() > 0 => Ok(seq[0].clone()),
        List(_, _) | Vector(_, _) | Nil => Ok(Nil),
        _ => error("invalid args to first"),
    }
}

fn rest(a: MalArgs) -> MalRet {
    match a[0] {
        List(ref seq, _) | Vector(ref seq, _) if seq.len() > 1 => Ok(list(seq[1..].to_vec())),
        List(_, _) | Vector(_, _) | Nil => Ok(list!()),
        _ => error("invalid args to first"),
    }
}

fn apply(a: MalArgs) -> MalRet {
    match a[a.len() - 1] {
        List(ref v, _) | Vector(ref v, _) => {
            let f = &a[0];
            let mut fargs = a[1..a.len() - 1].to_vec();
            fargs.extend_from_slice(v);
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
            Ok(list(res))
        }
        _ => error("map called with non-seq"),
    }
}

fn conj(a: MalArgs) -> MalRet {
    match a[0] {
        List(ref v, _) => {
            let sl = a[1..].iter().rev().cloned().collect::<Vec<MalVal>>();
            Ok(list([&sl[..], v].concat()))
        }
        Vector(ref v, _) => Ok(vector([v, &a[1..]].concat())),
        _ => error("conj: called with non-seq"),
    }
}

fn seq(a: MalArgs) -> MalRet {
    match a[0] {
        ref l @ List(ref v, _) if v.len() > 0 => Ok(l.clone()),
        Vector(ref v, _) if v.len() > 0 => Ok(list(v.to_vec())),
        Str(ref s) if !s.is_empty() => Ok(list(s.chars().map(|c| Str(c.to_string())).collect())),
        List(_, _) | Vector(_, _) | Str(_) | Nil => Ok(Nil),
        _ => error("seq: called with non-seq"),
    }
}

fn keyword(a: MalArgs) -> MalRet {
    match a[0] {
        Kwd(_) => Ok(a[0].clone()),
        Str(ref s) => Ok(Kwd(String::from(s))),
        _ => error("invalid type for keyword"),
    }
}

pub fn empty_q(a: MalArgs) -> MalRet {
    match a[0] {
        List(ref l, _) | Vector(ref l, _) => Ok(Bool(l.len() == 0)),
        Nil => Ok(Bool(true)),
        _ => error("invalid type for empty?"),
    }
}

pub fn count(a: MalArgs) -> MalRet {
    match a[0] {
        List(ref l, _) | Vector(ref l, _) => Ok(Int(l.len() as i64)),
        Nil => Ok(Int(0)),
        _ => error("invalid type for count"),
    }
}

pub fn atom(a: MalArgs) -> MalRet {
    Ok(Atom(Rc::new(std::cell::RefCell::new(a[0].clone()))))
}

pub fn deref(a: MalArgs) -> MalRet {
    match a[0] {
        Atom(ref a) => Ok(a.borrow().clone()),
        _ => error("attempt to deref a non-Atom"),
    }
}

pub fn reset_bang(a: MalArgs) -> MalRet {
    match a[0] {
        Atom(ref atm) => {
            *atm.borrow_mut() = a[1].clone();
            Ok(a[1].clone())
        }
        _ => error("attempt to reset! a non-Atom"),
    }
}

pub fn swap_bang(a: MalArgs) -> MalRet {
    match a[0] {
        Atom(ref atm) => {
            let mut fargs = a[2..].to_vec();
            fargs.insert(0, atm.borrow().clone());
            let result = a[1].apply(fargs)?;
            *atm.borrow_mut() = result.clone();
            Ok(result)
        }
        _ => error("attempt to swap! a non-Atom"),
    }
}

pub fn get_meta(a: MalArgs) -> MalRet {
    match a[0] {
        List(_, ref meta) | Vector(_, ref meta) | Hash(_, ref meta) => Ok((**meta).clone()),
        Func(_, ref meta) => Ok((**meta).clone()),
        MalFunc(FuncStruct { ref meta, .. }) => Ok((**meta).clone()),
        _ => error("meta not supported by type"),
    }
}

pub fn with_meta(a: MalArgs) -> MalRet {
    let m = Rc::new(a[1].clone());
    match a[0] {
        List(ref l, _) => Ok(List(l.clone(), m)),
        Vector(ref l, _) => Ok(Vector(l.clone(), m)),
        Hash(ref l, _) => Ok(Hash(l.clone(), m)),
        Func(ref l, _) => Ok(Func(*l, m)),
        MalFunc(ref f @ FuncStruct { .. }) => Ok(MalFunc(FuncStruct {
            meta: m,
            ..f.clone()
        })),
        _ => error("with-meta not supported by type"),
    }
}

pub fn ns() -> Vec<(&'static str, MalVal)> {
    vec![
        ("=", func(|a| Ok(Bool(a[0] == a[1])))),
        ("throw", func(|a| Err(a[0].clone()))),
        ("nil?", func(fn_is_type!(Nil))),
        ("true?", func(fn_is_type!(Bool(true)))),
        ("false?", func(fn_is_type!(Bool(false)))),
        ("symbol", func(symbol)),
        ("symbol?", func(fn_is_type!(Sym(_)))),
        ("string?", func(fn_is_type!(Str(_)))),
        ("keyword", func(keyword)),
        ("keyword?", func(fn_is_type!(Kwd(_)))),
        ("number?", func(fn_is_type!(Int(_)))),
        (
            "fn?",
            func(fn_is_type!(
                MalFunc(FuncStruct {
                    is_macro: false,
                    ..
                }),
                Func(_, _)
            )),
        ),
        (
            "macro?",
            func(fn_is_type!(MalFunc(FuncStruct { is_macro: true, .. }))),
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
        ("read-string", func(fn_str!(read_str))),
        ("readline", func(fn_str!(readline))),
        ("slurp", func(fn_str!(slurp))),
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
        ("list", func(|a| Ok(list(a)))),
        ("list?", func(fn_is_type!(List(_, _)))),
        ("vector", func(|a| Ok(vector(a)))),
        ("vector?", func(fn_is_type!(Vector(_, _)))),
        ("hash-map", func(hash_map)),
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
        ("empty?", func(empty_q)),
        ("nth", func(nth)),
        ("first", func(first)),
        ("rest", func(rest)),
        ("count", func(count)),
        ("apply", func(apply)),
        ("map", func(map)),
        ("conj", func(conj)),
        ("seq", func(seq)),
        ("meta", func(get_meta)),
        ("with-meta", func(with_meta)),
        ("atom", func(atom)),
        ("atom?", func(fn_is_type!(Atom(_)))),
        ("deref", func(deref)),
        ("reset!", func(reset_bang)),
        ("swap!", func(swap_bang)),
    ]
}
