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
    reader::read_str(str)
}

// eval
fn eval(ast: &MalVal, env: &Env) -> MalRet {
    // println!("EVAL: {}", print(&ast));
    match ast {
        Sym(sym) => Ok(env
            .get(sym)
            .ok_or_else(|| ErrString(format!("'{}' not found", sym)))?
            .clone()),
        Vector(v, _) => {
            let mut lst: MalArgs = vec![];
            for a in v.iter() {
                lst.push(eval(a, env)?);
            }
            Ok(vector!(lst))
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

fn rep(str: &str, env: &Env) -> Result<String, MalErr> {
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
    let mut rl = Editor::<(), rustyline::history::DefaultHistory>::new().unwrap();
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
                let _ = rl.add_history_entry(&line);
                rl.save_history(".mal-history").unwrap();
                if !line.is_empty() {
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
