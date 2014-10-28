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
        List(ref binds) | Vector(ref binds) => {
            match *mexprs {
                List(ref exprs) | Vector(ref exprs) => {
                    let mut it = binds.iter().enumerate();
                    for (i, b) in it {
                        match **b {
                            Sym(ref strn) => {
                                if *strn == "&".to_string() {
                                    variadic = true;
                                    break;
                                } else {
                                    env_set(env, strn.clone(), exprs[i].clone());
                                }
                            }
                            _ => return Err("non-symbol bind".to_string()),
                        }
                    }
                    if variadic {
                        let (i, sym) = it.next().unwrap();
                        match **sym {
                            Sym(ref s) => {
                                let rest = exprs.slice(i-1,exprs.len()).to_vec();
                                env_set(env, s.clone(), list(rest));
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

pub fn env_find(env: Env, key: String) -> Option<Env> {
    if env.borrow().data.contains_key(&key) {
        Some(env)
    } else {
        match env.borrow().outer {
            Some(ref e) => env_find(e.clone(), key),
            None => None,
        }
    }
}

pub fn env_root(env: &Env) -> Env {
    match env.borrow().outer {
        Some(ref ei) => env_root(ei),
        None => env.clone(),
    }
}

pub fn env_set(env: &Env, key: String, val: MalVal) {
    env.borrow_mut().data.insert(key, val.clone());
}

pub fn env_get(env: Env, key: String) -> MalRet {
    match env_find(env, key.clone()) {
        Some(e) => {
            match e.borrow().data.find_copy(&key) {
                Some(v) => Ok(v),
                None => Ok(_nil()),
            }
        },
        None    => err_string("'".to_string() + key + "' not found".to_string()),
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
