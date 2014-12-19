#![allow(dead_code)]

use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;

use types::{MalVal,MalRet,Sym,List,Vector,_nil,list,err_string};

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
                    for (i, b) in it {
                        match **b {
                            Sym(ref strn) => {
                                if *strn == "&".to_string() {
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
                                let rest = exprs.slice(i-1,exprs.len()).to_vec();
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

pub fn env_find(env: Env, key: MalVal) -> Option<Env> {
    match *key {
        Sym(ref k) => {
            if env.borrow().data.contains_key(k) {
                Some(env)
            } else {
                match env.borrow().outer {
                    Some(ref e) => env_find(e.clone(), key.clone()),
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
        Sym(ref k) => {
            env.borrow_mut().data.insert(k.to_string(), val.clone());
        },
        _ => {},
    }
}

pub fn env_get(env: Env, key: MalVal) -> MalRet {
    match *key {
        Sym(ref k) => {
            match env_find(env, key.clone()) {
                Some(e) => {
                    match e.borrow().data.find_copy(k) {
                        Some(v) => Ok(v),
                        None => Ok(_nil()),
                    }
                },
                None    => err_string("'".to_string() + k.to_string() + "' not found".to_string()),
            }
        }
        _ => err_string("env_get called with non-symbol key".to_string()),
    }
}

impl fmt::Show for EnvType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.outer {
            Some(ref o) => write!(f, "[{}/outer:{}]", self.data, o.borrow()),
            _ => write!(f, "{}", self.data)
        }
    }
}
