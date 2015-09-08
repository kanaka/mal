use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;

use types::{MalVal, MalRet, _nil, list, err_string};
use types::MalType::{Sym, List, Vector};

struct EnvType {
    data: HashMap<String,MalVal>,
    outer: Option<Env>,
}

pub type Env = Rc<RefCell<EnvType>>;

pub fn env_new(outer: Option<Env>) -> Env {
    Rc::new(RefCell::new(EnvType{data: HashMap::new(), outer: outer}))
}

pub fn env_bind(env: &Env,
                mbinds: MalVal,
                mexprs: MalVal) -> Result<Env,String> {
    let mut variadic = false;
    match *mbinds {
        List(ref binds,_) | Vector(ref binds,_) => {
            match *mexprs {
                List(ref exprs,_) | Vector(ref exprs,_) => {
                    let mut it = binds.iter().enumerate();
                    for (i, b) in it.by_ref() {
                        match **b {
                            Sym(ref strn) => {
                                if *strn == "&" {
                                    variadic = true;
                                    break;
                                } else {
                                    env_set(env, b.clone(), exprs[i].clone());
                                }
                            }
                            _ => return Err("non-symbol bind".to_string()),
                        }
                    }
                    if variadic {
                        let (i, sym) = it.next().unwrap();
                        match **sym {
                            Sym(_) => {
                                let rest = exprs[i-1..].to_vec();
                                env_set(env, sym.clone(), list(rest));
                            }
                            _ => return Err("& bind to non-symbol".to_string()),
                        }
                    }
                    Ok(env.clone())
                },
                _ => Err("exprs must be a list".to_string()),
            }
        },
        _ => Err("binds must be a list".to_string()),
    }
}

pub fn env_find(env: &Env, key: &MalVal) -> Option<Env> {
    match **key {
        Sym(ref k) => {
            let map = env.borrow();
            if map.data.contains_key(k) {
                Some(env.clone())
            } else {
                match map.outer {
                    Some(ref e) => env_find(e, key),
                    None => None,
                }
            }
        },
        _ => None
    }
}

pub fn env_root(env: &Env) -> Env {
    match env.borrow().outer {
        Some(ref ei) => env_root(ei),
        None => env.clone(),
    }
}

pub fn env_set(env: &Env, key: MalVal, val: MalVal) {
    match *key {
        Sym(ref k) => { env.borrow_mut().data.insert(k.to_string(), val); }
        _ => {},
    }
}

pub fn env_get(env: &Env, key: &MalVal) -> MalRet {
    match **key {
        Sym(ref k) => {
            match env_find(env, key) {
                Some(e) => {
                    match e.borrow().data.get(k) {
                        Some(v) => Ok(v.clone()),
                        None => Ok(_nil()),
                    }
                },
                None => err_string(format!("'{}' not found", k)),
            }
        }
        _ => err_string("env_get called with non-symbol key".to_string()),
    }
}
