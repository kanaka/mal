use std::rc::Rc;
//use std::collections::HashMap;
use fnv::FnvHashMap;
use itertools::Itertools;

#[macro_use]
extern crate lazy_static;
extern crate fnv;
extern crate itertools;
extern crate regex;

extern crate rustyline;
use rustyline::error::ReadlineError;
use rustyline::Editor;

#[macro_use]
mod types;
use crate::types::MalVal::{Bool, Hash, List, MalFunc, Nil, Sym, Vector};
use crate::types::{error, format_error, MalArgs, MalErr, MalRet, MalVal};
mod env;
mod printer;
mod reader;
use crate::env::{env_get, env_new, env_set, env_sets, Env};
#[macro_use]
mod core;

// read
fn read(str: &str) -> MalRet {
    reader::read_str(str)
}

// eval
fn eval(ast: &MalVal, env: &Env) -> MalRet {
    match env_get(env, "DEBUG-EVAL") {
        None | Some(Bool(false)) | Some(Nil) => (),
        _ => println!("EVAL: {}", print(ast)),
    }
    match ast {
        Sym(s) => match env_get(env, s) {
            Some(r) => Ok(r),
            None => error (&format!("'{}' not found", s)),
        }
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
            match a0 {
                Sym(a0sym) if a0sym == "def!" => {
                    env_set(env, &l[1], eval(&l[2], env)?)
                }
                Sym(a0sym) if a0sym == "let*" => {
                    let let_env = &env_new(Some(env.clone()));
                    let (a1, a2) = (&l[1], &l[2]);
                    match a1 {
                        List(binds, _) | Vector(binds, _) => {
                            for (b, e) in binds.iter().tuples() {
                                let val = eval(e, let_env)?;
                                env_set(let_env, b, val)?;
                            }
                        }
                        _ => {
                            return error("let* with non-List bindings");
                        }
                    };
                    eval(a2, let_env)
                }
                Sym(a0sym) if a0sym == "do" => {
                    for i in 1..l.len() - 1 {
                        let _ = eval(&l[i], env)?;
                    }
                    eval(l.last().unwrap_or(&Nil), env)
                }
                Sym(a0sym) if a0sym == "if" => {
                    let cond = eval(&l[1], env)?;
                    match cond {
                        Bool(false) | Nil if l.len() >= 4 => eval(&l[3], env),
                        Bool(false) | Nil => Ok(Nil),
                        _ if l.len() >= 3 => eval(&l[2], env),
                        _ => Ok(Nil),
                    }
                }
                Sym(a0sym) if a0sym == "fn*" => {
                    let (a1, a2) = (l[1].clone(), l[2].clone());
                    Ok(MalFunc {
                        eval,
                        ast: Rc::new(a2),
                        env: env.clone(),
                        params: Rc::new(a1),
                        is_macro: false,
                        meta: Rc::new(Nil),
                    })
                }
                _ => {
                    let f = eval(a0, env)?;
                    let mut args: MalArgs = vec![];
                    for i in 1..l.len() {
                        args.push(eval(&l[i], env)?);
                    }
                    f.apply(args)
                },
            }
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

fn re(str: &str, env: &Env) {
    if let Ok(ast) = read(str) {
        if eval(&ast, env).is_ok() {
            return;
        }
    }
    panic!("error during startup");
}

fn main() {
    // `()` can be used when no completer is required
    let mut rl = Editor::<(), rustyline::history::DefaultHistory>::new().unwrap();
    if rl.load_history(".mal-history").is_err() {
        eprintln!("No previous history.");
    }

    // core.rs: defined using rust
    let repl_env = env_new(None);
    for (k, v) in core::ns() {
        env_sets(&repl_env, k, v);
    }

    // core.mal: defined using the language itself
    re("(def! not (fn* (a) (if a false true)))", &repl_env);

    // main repl loop
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
