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
    reader::read_str(str.to_string())
}

// eval
fn eval_ast(ast: &MalVal, env: &Env) -> MalRet {
    match ast {
        Sym(_) => Ok(env_get(&env, &ast)?),
        List(v, _) => {
            let mut lst: MalArgs = vec![];
            for a in v.iter() {
                lst.push(eval(a.clone(), env.clone())?)
            }
            Ok(list!(lst))
        }
        Vector(v, _) => {
            let mut lst: MalArgs = vec![];
            for a in v.iter() {
                lst.push(eval(a.clone(), env.clone())?)
            }
            Ok(vector!(lst))
        }
        Hash(hm, _) => {
            let mut new_hm: FnvHashMap<String, MalVal> = FnvHashMap::default();
            for (k, v) in hm.iter() {
                new_hm.insert(k.to_string(), eval(v.clone(), env.clone())?);
            }
            Ok(Hash(Rc::new(new_hm), Rc::new(Nil)))
        }
        _ => Ok(ast.clone()),
    }
}

fn eval(ast: MalVal, env: Env) -> MalRet {
    match ast.clone() {
        List(l, _) => {
            if l.len() == 0 {
                return Ok(ast);
            }
            let a0 = &l[0];
            match a0 {
                Sym(ref a0sym) if a0sym == "def!" => {
                    env_set(&env, l[1].clone(), eval(l[2].clone(), env.clone())?)
                }
                Sym(ref a0sym) if a0sym == "let*" => {
                    let let_env = env_new(Some(env.clone()));
                    let (a1, a2) = (l[1].clone(), l[2].clone());
                    match a1 {
                        List(ref binds, _) | Vector(ref binds, _) => {
                            for (b, e) in binds.iter().tuples() {
                                match b {
                                    Sym(_) => {
                                        let _ = env_set(
                                            &let_env,
                                            b.clone(),
                                            eval(e.clone(), let_env.clone())?,
                                        );
                                    }
                                    _ => {
                                        return error("let* with non-Sym binding");
                                    }
                                }
                            }
                        }
                        _ => {
                            return error("let* with non-List bindings");
                        }
                    };
                    eval(a2, let_env)
                }
                Sym(ref a0sym) if a0sym == "do" => match eval_ast(&list!(l[1..].to_vec()), &env)? {
                    List(el, _) => Ok(el.last().unwrap_or(&Nil).clone()),
                    _ => error("invalid do form"),
                },
                Sym(ref a0sym) if a0sym == "if" => {
                    let cond = eval(l[1].clone(), env.clone())?;
                    match cond {
                        Bool(false) | Nil if l.len() >= 4 => eval(l[3].clone(), env.clone()),
                        Bool(false) | Nil => Ok(Nil),
                        _ if l.len() >= 3 => eval(l[2].clone(), env.clone()),
                        _ => Ok(Nil),
                    }
                }
                Sym(ref a0sym) if a0sym == "fn*" => {
                    let (a1, a2) = (l[1].clone(), l[2].clone());
                    Ok(MalFunc {
                        eval: eval,
                        ast: Rc::new(a2),
                        env: env,
                        params: Rc::new(a1),
                        is_macro: false,
                        meta: Rc::new(Nil),
                    })
                }
                _ => match eval_ast(&ast, &env)? {
                    List(ref el, _) => {
                        let ref f = el[0].clone();
                        f.apply(el[1..].to_vec())
                    }
                    _ => error("expected a list"),
                },
            }
        }
        _ => eval_ast(&ast, &env),
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

fn main() {
    // `()` can be used when no completer is required
    let mut rl = Editor::<()>::new();
    if rl.load_history(".mal-history").is_err() {
        eprintln!("No previous history.");
    }

    // core.rs: defined using rust
    let repl_env = env_new(None);
    for (k, v) in core::ns() {
        env_sets(&repl_env, k, v);
    }

    // core.mal: defined using the language itself
    let _ = rep("(def! not (fn* (a) (if a false true)))", &repl_env);

    // main repl loop
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
