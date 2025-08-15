use std::cell::RefCell;
use std::rc::Rc;
//use std::collections::HashMap;
use fnv::FnvHashMap;
use itertools::Itertools;

use crate::env::Env;
use crate::types::MalVal::{Bool, Func, Hash, Int, Kwd, List, MalFunc, Nil, Str, Sym, Vector};

// Function closures and atoms may create cyclic dependencies, so
// reference counting should be replaced at least for these two kinds
// of references.

#[derive(Clone)]
pub enum MalVal {
    Nil,
    Bool(bool),
    Int(i64),
    //Float(f64),
    Str(String),
    Sym(String),
    Kwd(String),
    List(Rc<Vec<MalVal>>, Rc<MalVal>),
    Vector(Rc<Vec<MalVal>>, Rc<MalVal>),
    Hash(Rc<FnvHashMap<String, MalVal>>, Rc<MalVal>),
    Func(fn(MalArgs) -> MalRet, Rc<MalVal>),
    MalFunc(FuncStruct),
    Atom(Rc<RefCell<MalVal>>),
}

#[derive(Clone)]
pub struct FuncStruct {
    pub ast: Rc<MalVal>,
    pub env: Env,
    pub params: Rc<MalVal>,
    pub is_macro: bool,
    pub meta: Rc<MalVal>,
}

pub type MalArgs = Vec<MalVal>;
pub type MalRet = Result<MalVal, MalVal>;

// type utility macros

macro_rules! list {
  [$($args:expr),*] => {{
    let v: Vec<MalVal> = vec![$($args),*];
    List(Rc::new(v),Rc::new(Nil))
  }}
}

// type utility functions

pub fn error<T>(s: &str) -> Result<T, MalVal> {
    Err(Str(s.to_string()))
}

pub fn list(seq: MalArgs) -> MalVal {
    List(Rc::new(seq), Rc::new(Nil))
}

pub fn vector(seq: MalArgs) -> MalVal {
    Vector(Rc::new(seq), Rc::new(Nil))
}

impl PartialEq for MalVal {
    fn eq(&self, other: &MalVal) -> bool {
        match (self, other) {
            (Nil, Nil) => true,
            (Bool(ref a), Bool(ref b)) => a == b,
            (Int(ref a), Int(ref b)) => a == b,
            (Str(ref a), Str(ref b)) => a == b,
            (Sym(ref a), Sym(ref b)) => a == b,
            (Kwd(ref a), Kwd(ref b)) => a == b,
            (List(ref a, _), List(ref b, _))
            | (Vector(ref a, _), Vector(ref b, _))
            | (List(ref a, _), Vector(ref b, _))
            | (Vector(ref a, _), List(ref b, _)) => a == b,
            (Hash(ref a, _), Hash(ref b, _)) => a == b,
            (MalFunc { .. }, MalFunc { .. }) => false,
            _ => false,
        }
    }
}

pub fn func(f: fn(MalArgs) -> MalRet) -> MalVal {
    Func(f, Rc::new(Nil))
}

pub fn _assoc(mut hm: FnvHashMap<String, MalVal>, kvs: MalArgs) -> MalRet {
    if kvs.len() % 2 != 0 {
        return error("odd number of elements");
    }
    for (k, v) in kvs.iter().tuples() {
        hm.insert(wrap_map_key(k)?, v.clone());
    }
    Ok(Hash(Rc::new(hm), Rc::new(Nil)))
}

pub fn wrap_map_key(k: &MalVal) -> Result<String, MalVal> {
    match k {
        Str(s) => Ok(String::from(s)),
        Kwd(s) => Ok(format!("\u{29e}{}", s)),
        _ => error("key is not string"),
    }
}

pub fn unwrap_map_key(s: &str) -> MalVal {
    match s.strip_prefix('\u{29e}') {
        Some(keyword) => Kwd(String::from(keyword)),
        _ => Str(String::from(s)),
    }
}

pub fn hash_map(kvs: MalArgs) -> MalRet {
    let hm: FnvHashMap<String, MalVal> = FnvHashMap::default();
    _assoc(hm, kvs)
}
