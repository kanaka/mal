use std::rc::Rc;
//use std::collections::HashMap;
use fnv::FnvHashMap;

#[macro_use]
extern crate lazy_static;
extern crate fnv;
extern crate itertools;
extern crate regex;

extern crate rustyline;
use rustyline::error::ReadlineError;
use rustyline::Editor;

#[macro_use]
#[allow(dead_code)]
mod types;
use crate::types::MalErr::ErrString;
use crate::types::MalVal::{Hash, Int, List, Nil, Sym, Vector};
use crate::types::{error, format_error, func, MalArgs, MalErr, MalRet, MalVal};
mod printer;
mod reader;
// TODO: figure out a way to avoid including env
#[allow(dead_code)]
mod env;

pub type Env = FnvHashMap<String, MalVal>;

// read
fn read(str: &str) -> MalRet {
    reader::read_str(str.to_string())
}

// eval
fn eval_ast(v: &MalArgs, env: &Env) -> Result<MalArgs, MalErr> {
            let mut lst: MalArgs = vec![];
            for a in v.iter() {
                match eval(a.clone(), env.clone()) {
                    Ok(elt) => lst.push(elt),
                    Err(e) => return Err(e),
                }
            }
            return Ok(lst);
}

fn eval(ast: MalVal, env: Env) -> MalRet {
    // println!("EVAL: {}", print(&ast)),
    match ast {
        Sym(sym) => Ok(env
            .get(&sym)
            .ok_or(ErrString(format!("'{}' not found", sym)))?
            .clone()),
        Vector(ref v, _) => match eval_ast(&v, &env) {
            Ok(lst) => Ok(vector!(lst)),
            Err(e) => Err(e),
        }
        Hash(hm, _) => {
            let mut new_hm: FnvHashMap<String, MalVal> = FnvHashMap::default();
            for (k, v) in hm.iter() {
                new_hm.insert(k.to_string(), eval(v.clone(), env.clone())?);
            }
            Ok(Hash(Rc::new(new_hm), Rc::new(Nil)))
        }
        List(ref l, _) => {
            if l.len() == 0 {
                return Ok(ast);
            }
            match eval_ast(&l, &env) {
                Ok(el) => {
                    let ref f = el[0].clone();
                    f.apply(el[1..].to_vec())
                }
                Err(e) => return Err(e),
            }
        }
        _ => Ok(ast),
    }
}

// print
fn print(ast: &MalVal) -> String {
    ast.pr_str(true)
}

fn rep(str: &str, env: &Env) -> Result<String, MalErr> {
    let ast = read(str)?;
    let exp = eval(ast, env.clone())?;
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
    let mut rl = Editor::<()>::new();
    if rl.load_history(".mal-history").is_err() {
        eprintln!("No previous history.");
    }

    let mut repl_env = Env::default();
    repl_env.insert("+".to_string(), func(|a: MalArgs| int_op(|i, j| i + j, a)));
    repl_env.insert("-".to_string(), func(|a: MalArgs| int_op(|i, j| i - j, a)));
    repl_env.insert("*".to_string(), func(|a: MalArgs| int_op(|i, j| i * j, a)));
    repl_env.insert("/".to_string(), func(|a: MalArgs| int_op(|i, j| i / j, a)));

    loop {
        let readline = rl.readline("user> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(&line);
                rl.save_history(".mal-history").unwrap();
                if line.len() > 0 {
                    match rep(&line, &repl_env) {
                        Ok(out) => println!("{}", out),
                        Err(e) => println!("Error: {}", format_error(e)),
                    }
                }
            }
            Err(ReadlineError::Interrupted) => continue,
            Err(ReadlineError::Eof) => break,
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
}
