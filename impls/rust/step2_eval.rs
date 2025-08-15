#![allow(non_snake_case)]

use std::rc::Rc;
//use std::collections::HashMap;
use fnv::FnvHashMap;

extern crate fnv;
extern crate itertools;
extern crate regex;

mod readline;
#[macro_use]
#[allow(dead_code)]
mod types;
use crate::types::MalVal::{Func, Hash, Int, List, Nil, Sym, Vector};
use crate::types::{error, func, vector, MalArgs, MalRet, MalVal};
#[allow(dead_code)]
mod env;
mod printer;
mod reader;

pub type Env = FnvHashMap<String, MalVal>;

impl MalVal {
    pub fn apply(&self, args: MalArgs) -> MalRet {
        match self {
            Func(f, _) => f(args),
            _ => error("attempt to call non-function"),
        }
    }
}

// read
fn read(str: &str) -> MalRet {
    reader::read_str(str)
}

// eval
fn eval(ast: &MalVal, env: &Env) -> MalRet {
    // println!("EVAL: {}", print(&ast));
    match ast {
        Sym(s) => match env.get(s) {
            Some(r) => Ok(r.clone()),
            None => error(&format!("'{}' not found", s)),
        },
        Vector(v, _) => {
            let mut lst: MalArgs = vec![];
            for a in v.iter() {
                lst.push(eval(a, env)?);
            }
            Ok(vector(lst))
        }
        Hash(hm, _) => {
            let mut new_hm: FnvHashMap<String, MalVal> = FnvHashMap::default();
            for (k, v) in hm.iter() {
                new_hm.insert(k.to_string(), eval(v, env)?);
            }
            Ok(Hash(Rc::new(new_hm), Rc::new(Nil)))
        }
        List(l, _) => {
            if l.is_empty() {
                return Ok(ast.clone());
            }
            let a0 = &l[0];
            let f = eval(a0, env)?;
            let mut args: MalArgs = vec![];
            for i in 1..l.len() {
                args.push(eval(&l[i], env)?);
            }
            f.apply(args)
        }
        _ => Ok(ast.clone()),
    }
}

// print
fn print(ast: &MalVal) -> String {
    ast.pr_str(true)
}

fn rep(str: &str, env: &Env) -> Result<String, MalVal> {
    let ast = read(str)?;
    let exp = eval(&ast, env)?;
    Ok(print(&exp))
}

fn int_op(op: fn(i64, i64) -> i64, a: MalArgs) -> MalRet {
    match (a[0].clone(), a[1].clone()) {
        (Int(a0), Int(a1)) => Ok(Int(op(a0, a1))),
        _ => error("invalid int_op args"),
    }
}

fn main() {
    // `()` can be used when no completer is required

    let mut repl_env = Env::default();
    repl_env.insert("+".to_string(), func(|a: MalArgs| int_op(|i, j| i + j, a)));
    repl_env.insert("-".to_string(), func(|a: MalArgs| int_op(|i, j| i - j, a)));
    repl_env.insert("*".to_string(), func(|a: MalArgs| int_op(|i, j| i * j, a)));
    repl_env.insert("/".to_string(), func(|a: MalArgs| int_op(|i, j| i / j, a)));

    // main repl loop
    while let Some(ref line) = readline::readline("user> ") {
        if !line.is_empty() {
            match rep(line, &repl_env) {
                Ok(ref out) => println!("{}", out),
                Err(ref e) => println!("Error: {}", e.pr_str(true)),
            }
        }
    }
    println!();
}
