#![allow(dead_code)]

use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use super::printer::{escape_str,pr_list};
use super::env::{Env,env_new,env_bind};

#[deriving(Clone)]
#[allow(non_camel_case_types)]
pub enum MalType {
    Nil,
    True,
    False,
    Int(int),
    Strn(String),
    Sym(String),
    List(Vec<MalVal>, MalVal),
    Vector(Vec<MalVal>, MalVal),
    Hash_Map(HashMap<String, MalVal>, MalVal),
    Func(fn(Vec<MalVal>) -> MalRet, MalVal),
    //Func(fn(&[MalVal]) -> MalRet),
    //Func(|Vec<MalVal>|:'a -> MalRet),
    MalFunc(MalFuncData, MalVal),
    Atom(RefCell<MalVal>),
}

pub type MalVal = Rc<MalType>;

#[deriving(Show)]
pub enum MalError {
    ErrString(String),
    ErrMalVal(MalVal),
}

pub type MalRet = Result<MalVal,MalError>;


pub fn err_string(s: String) -> MalRet {
    Err(ErrString(s))
}

pub fn err_str(s: &str) -> MalRet {
    Err(ErrString(s.to_string()))
}

pub fn err_val(mv: MalVal) -> MalRet {
    Err(ErrMalVal(mv))
}

/*
pub enum MalRet {
    Val(MalVal),
    MalErr(MalVal),
    StringErr(String),
}
*/


#[deriving(Clone)]
pub struct MalFuncData {
    pub eval:     fn(MalVal, Env) -> MalRet,
    pub exp:      MalVal,
    pub env:      Env,
    pub params:   MalVal,
    pub is_macro: bool,
}

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
                if v.as_slice().starts_with("\u029e") {
                    res.push_str(":");
                    res.push_str(v.as_slice().slice(2,v.len()))
                } else if print_readably {
                    res.push_str(escape_str((*v).as_slice()).as_slice())
                } else {
                    res.push_str(v.as_slice())
                }
            },
            List(ref v,_) => {
                res = pr_list(v, _r, "(", ")", " ")
            },
            Vector(ref v,_) => {
                res = pr_list(v, _r, "[", "]", " ")
            },
            Hash_Map(ref v,_) => {
                let mut first = true;
                res.push_str("{");
                for (key, value) in v.iter() {
                    if first { first = false; } else { res.push_str(" "); }
                    if key.as_slice().starts_with("\u029e") {
                        res.push_str(":");
                        res.push_str(key.as_slice().slice(2,key.len()))
                    } else if print_readably {
                        res.push_str(escape_str(key.as_slice()).as_slice())
                    } else {
                        res.push_str(key.as_slice())
                    }
                    res.push_str(" ");
                    res.push_str(value.pr_str(_r).as_slice());
                }
                res.push_str("}")
            },
            // TODO: better native function representation
            Func(_,_) => {
                res.push_str(format!("#<function ...>").as_slice())
            },
            MalFunc(ref mf,_) => {
                res.push_str(format!("(fn* {} {})", mf.params, mf.exp).as_slice())
            },
            Atom(ref v) => {
                res = format!("(atom {})", v.borrow());
            },
        };
        res
    }

    pub fn apply(&self, args:Vec<MalVal>) -> MalRet {
        match *self {
            Func(f,_) => f(args),
            MalFunc(ref mf,_) => {
                let mfc = mf.clone();
                let alst = list(args);
                let new_env = env_new(Some(mfc.env.clone()));
                match env_bind(&new_env, mfc.params, alst) {
                    Ok(_) => (mfc.eval)(mfc.exp, new_env),
                    Err(e) => err_string(e),
                }
            },
            _ => err_str("attempt to call non-function"),
        }

    }
}

impl PartialEq for MalType {
    fn eq(&self, other: &MalType) -> bool {
        match (self, other) {
            (&Nil, &Nil) |
            (&True, &True) |
            (&False, &False) => true,
            (&Int(ref a), &Int(ref b)) => a == b,
            (&Strn(ref a), &Strn(ref b)) => a == b,
            (&Sym(ref a), &Sym(ref b)) => a == b,
            (&List(ref a,_), &List(ref b,_)) |
            (&Vector(ref a,_), &Vector(ref b,_)) |
            (&List(ref a,_), &Vector(ref b,_)) |
            (&Vector(ref a,_), &List(ref b,_)) => a == b,
            (&Hash_Map(ref a,_), &Hash_Map(ref b,_)) => a == b,
            // TODO: fix this
            (&Func(_,_), &Func(_,_)) => false,
            (&MalFunc(_,_), &MalFunc(_,_)) => false,
            _ => return false,
        } 
    }
}

impl fmt::Show for MalType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.pr_str(true))
    }
}


// Scalars
pub fn _nil() -> MalVal { Rc::new(Nil) }
pub fn nil_q(a:Vec<MalVal>) -> MalRet {
    if a.len() != 1 {
        return err_str("Wrong arity to nil? call");
    }
    match *a[0].clone() {
        Nil => Ok(_true()),
        _   => Ok(_false()),
    }
}

pub fn _true() -> MalVal { Rc::new(True) }
pub fn true_q(a:Vec<MalVal>) -> MalRet {
    if a.len() != 1 {
        return err_str("Wrong arity to true? call");
    }
    match *a[0].clone() {
        True => Ok(_true()),
        _    => Ok(_false()),
    }
}

pub fn _false() -> MalVal { Rc::new(False) }
pub fn false_q(a:Vec<MalVal>) -> MalRet {
    if a.len() != 1 {
        return err_str("Wrong arity to false? call");
    }
    match *a[0].clone() {
        False => Ok(_true()),
        _     => Ok(_false()),
    }
}

pub fn _int(i: int) -> MalVal { Rc::new(Int(i)) }


// Symbols
pub fn symbol(strn: &str) -> MalVal { Rc::new(Sym(strn.to_string())) }
pub fn _symbol(a: Vec<MalVal>) -> MalRet {
    if a.len() != 1 {
        return err_str("Wrong arity to symbol call");
    }
    match *a[0].clone() {
        Strn(ref s) => {
            Ok(Rc::new(Sym(s.to_string())))
        },
        _ => return err_str("symbol called on non-string"),
    }
}
pub fn symbol_q(a:Vec<MalVal>) -> MalRet {
    if a.len() != 1 {
        return err_str("Wrong arity to symbol? call");
    }
    match *a[0].clone() {
        Sym(_) => Ok(_true()),
        _      => Ok(_false()),
    }
}

// Keywords
pub fn _keyword(a: Vec<MalVal>) -> MalRet {
    if a.len() != 1 {
        return err_str("Wrong arity to keyword call");
    }
    match *a[0].clone() {
        Strn(ref s) => {
            Ok(Rc::new(Strn("\u029e".to_string() + s.to_string())))
        },
        _ => return err_str("keyword called on non-string"),
    }
}
pub fn keyword_q(a:Vec<MalVal>) -> MalRet {
    if a.len() != 1 {
        return err_str("Wrong arity to keyword? call");
    }
    match *a[0].clone() {
        Strn(ref s) => {
            if s.as_slice().starts_with("\u029e") {
                Ok(_true())
            } else {
                Ok(_false())
            }
        },
        _ => Ok(_false()),
    }
}


// Strings
pub fn strn(strn: &str) -> MalVal { Rc::new(Strn(strn.to_string())) }
pub fn string(strn: String) -> MalVal { Rc::new(Strn(strn)) }

// Lists
pub fn list(seq: Vec<MalVal>) -> MalVal { Rc::new(List(seq,_nil())) }
pub fn listm(seq: Vec<MalVal>, meta: MalVal) -> MalVal {
    Rc::new(List(seq,meta))
}
pub fn listv(seq:Vec<MalVal>) -> MalRet { Ok(list(seq)) }
pub fn list_q(a:Vec<MalVal>) -> MalRet {
    if a.len() != 1 {
        return err_str("Wrong arity to list? call");
    }
    match *a[0].clone() {
        List(_,_) => Ok(_true()),
        _ => Ok(_false()),
    }
}

// Vectors
pub fn vector(seq: Vec<MalVal>) -> MalVal { Rc::new(Vector(seq,_nil())) }
pub fn vectorm(seq: Vec<MalVal>, meta: MalVal) -> MalVal {
    Rc::new(Vector(seq,meta))
}
pub fn vectorv(seq: Vec<MalVal>) -> MalRet { Ok(vector(seq)) }
pub fn vector_q(a:Vec<MalVal>) -> MalRet {
    if a.len() != 1 {
        return err_str("Wrong arity to vector? call");
    }
    match *a[0].clone() {
        Vector(_,_) => Ok(_true()),
        _           => Ok(_false()),
    }
}

// Hash Maps
pub fn hash_map(hm: HashMap<String,MalVal>) -> MalVal {
    Rc::new(Hash_Map(hm,_nil()))
}
pub fn hash_mapm(hm: HashMap<String,MalVal>, meta: MalVal) -> MalVal {
    Rc::new(Hash_Map(hm,meta))
}
pub fn _assoc(hm: &HashMap<String,MalVal>, a:Vec<MalVal>) -> MalRet {
    if a.len() % 2 == 1 {
        return err_str("odd number of hash-map keys/values");
    }
    let mut new_hm = hm.clone();
    let mut it = a.iter();
    loop {
        let k = match it.next() {
            Some(mv) => match *mv.clone() {
                Strn(ref s) => s.to_string(),
                _ => return err_str("key is not a string in hash-map call"),
            },
            None => break,
        };
        let v = it.next().unwrap();
        new_hm.insert(k, v.clone());
    }
    Ok(Rc::new(Hash_Map(new_hm,_nil())))
}
pub fn _dissoc(hm: &HashMap<String,MalVal>, a:Vec<MalVal>) -> MalRet {
    let mut new_hm = hm.clone();
    let mut it = a.iter();
    loop {
        let k = match it.next() {
            Some(mv) => match *mv.clone() {
                Strn(ref s) => s.to_string(),
                _ => return err_str("key is not a string in hash-map call"),
            },
            None => break,
        };
        new_hm.remove(&k);
    }
    Ok(Rc::new(Hash_Map(new_hm,_nil())))
}
pub fn hash_mapv(seq: Vec<MalVal>) -> MalRet {
    let new_hm: HashMap<String,MalVal> = HashMap::new();
    _assoc(&new_hm, seq)
}
pub fn hash_map_q(a:Vec<MalVal>) -> MalRet {
    if a.len() != 1 {
        return err_str("Wrong arity to map? call");
    }
    match *a[0].clone() {
        Hash_Map(_,_) => Ok(_true()),
        _             => Ok(_false()),
    }
}

// Functions
pub fn func(f: fn(Vec<MalVal>) -> MalRet) -> MalVal {
    Rc::new(Func(f, _nil()))
}
pub fn funcm(f: fn(Vec<MalVal>) -> MalRet, meta: MalVal) -> MalVal {
    Rc::new(Func(f, meta))
}
pub fn malfunc(eval: fn(MalVal, Env) -> MalRet,
               exp: MalVal,
               env: Env,
               params: MalVal,
               meta: MalVal) -> MalVal {
    Rc::new(MalFunc(MalFuncData{eval: eval,
                                exp: exp,
                                env: env,
                                params: params,
                                is_macro: false},meta))
}
pub fn malfuncd(mfd: MalFuncData, meta: MalVal) -> MalVal {
    Rc::new(MalFunc(mfd,meta))
}


// Atoms
pub fn atom_q(a:Vec<MalVal>) -> MalRet {
    if a.len() != 1 {
        return err_str("Wrong arity to atom? call");
    }
    match *a[0].clone() {
        Atom(_) => Ok(_true()),
        _       => Ok(_false()),
    }
}
pub fn atom(a:Vec<MalVal>) -> MalRet {
    if a.len() != 1 {
        return err_str("Wrong arity to atom call");
    }
    Ok(Rc::new(Atom(RefCell::new(a[0].clone()))))
}


// General functions
pub fn sequential_q(a:Vec<MalVal>) -> MalRet {
    if a.len() != 1 {
        return err_str("Wrong arity to sequential? call");
    }
    match *a[0].clone() {
        List(_,_) | Vector(_,_) => Ok(_true()),
        _                       => Ok(_false()),
    }
}
