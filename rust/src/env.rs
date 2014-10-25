use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;

use types::{MalVal,MalRet,Nil};

struct EnvType {
    data: HashMap<String,MalVal>,
    outer: Option<Env>,
}

pub type Env = Rc<RefCell<EnvType>>;

pub fn env_new(outer: Option<Env>) -> Env {
    Rc::new(RefCell::new(EnvType{data: HashMap::new(), outer: outer}))
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

pub fn env_set(env: &Env, key: String, val: MalVal) {
    env.borrow_mut().data.insert(key, val.clone());
}

pub fn env_get(env: Env, key: String) -> MalRet {
    match env_find(env, key.clone()) {
        Some(e) => {
            match e.borrow().data.find_copy(&key) {
                Some(v) => Ok(v),
                None => Ok(Rc::new(Nil)),
            }
        },
        None    => Ok(Rc::new(Nil)),
    }
}

