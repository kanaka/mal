#![allow(non_snake_case)]

use std::rc::Rc;
//use std::collections::HashMap;
use fnv::FnvHashMap;
use itertools::Itertools;

extern crate fnv;
extern crate itertools;
extern crate regex;

mod readline;
#[macro_use]
mod types;
use crate::types::MalVal::{Bool, Func, Hash, List, MalFunc, Nil, Str, Sym, Vector};
use crate::types::{error, list, vector, FuncStruct, MalArgs, MalRet, MalVal};
mod env;
mod printer;
mod reader;
use crate::env::{env_bind, env_get, env_new, env_set, env_sets, Env};
#[macro_use]
mod core;

impl MalVal {
    pub fn apply(&self, args: MalArgs) -> MalRet {
        match self {
            Func(f, _) => f(args),
            MalFunc(FuncStruct {
                ref ast,
                ref env,
                ref params,
                ..
            }) => {
                let fn_env = &env_bind(env.clone(), params, args)?;
                eval(ast, fn_env)
            }
            _ => error("attempt to call non-function"),
        }
    }
}

// read
fn read(str: &str) -> MalRet {
    reader::read_str(str)
}

// eval

fn qq_iter(elts: &MalArgs) -> MalVal {
    let mut acc = list!();
    for elt in elts.iter().rev() {
        if let List(v, _) = elt {
            if v.len() == 2 {
                if let Sym(ref s) = v[0] {
                    if s == "splice-unquote" {
                        acc = list!(Sym("concat".to_string()), v[1].clone(), acc);
                        continue;
                    }
                }
            }
        }
        acc = list!(Sym("cons".to_string()), quasiquote(elt), acc);
    }
    acc
}

fn quasiquote(ast: &MalVal) -> MalVal {
    match ast {
        List(v, _) => {
            if v.len() == 2 {
                if let Sym(ref s) = v[0] {
                    if s == "unquote" {
                        return v[1].clone();
                    }
                }
            }
            qq_iter(v)
        }
        Vector(v, _) => list!(Sym("vec".to_string()), qq_iter(v)),
        Hash(_, _) | Sym(_) => list!(Sym("quote".to_string()), ast.clone()),
        _ => ast.clone(),
    }
}

fn eval(orig_ast: &MalVal, orig_env: &Env) -> MalRet {
    let mut ast = orig_ast;
    let mut env = orig_env;
    // These variables ensure a sufficient lifetime for the data
    // referenced by ast and env.
    let mut live_ast;
    let mut live_env;

    'tco: loop {
        match env_get(env, "DEBUG-EVAL") {
            None | Some(Bool(false)) | Some(Nil) => (),
            _ => println!("EVAL: {}", print(ast)),
        }
        match ast {
            Sym(s) => match env_get(env, s) {
                Some(r) => return Ok(r),
                None => return error(&format!("'{}' not found", s)),
            },
            Vector(v, _) => {
                let mut lst: MalArgs = vec![];
                for a in v.iter() {
                    lst.push(eval(a, env)?);
                }
                return Ok(vector(lst));
            }
            Hash(hm, _) => {
                let mut new_hm: FnvHashMap<String, MalVal> = FnvHashMap::default();
                for (k, v) in hm.iter() {
                    new_hm.insert(k.to_string(), eval(v, env)?);
                }
                return Ok(Hash(Rc::new(new_hm), Rc::new(Nil)));
            }
            List(l, _) => {
                if l.is_empty() {
                    return Ok(ast.clone());
                }
                let a0 = &l[0];
                match a0 {
                    Sym(a0sym) if a0sym == "def!" => {
                        return env_set(env, &l[1], eval(&l[2], env)?);
                    }
                    Sym(a0sym) if a0sym == "let*" => {
                        live_env = env_new(Some(env.clone()));
                        env = &live_env;
                        let (a1, a2) = (&l[1], &l[2]);
                        match a1 {
                            List(binds, _) | Vector(binds, _) => {
                                for (b, e) in binds.iter().tuples() {
                                    let val = eval(e, env)?;
                                    env_set(env, b, val)?;
                                }
                            }
                            _ => {
                                return error("let* with non-List bindings");
                            }
                        };
                        live_ast = a2.clone();
                        ast = &live_ast;
                        continue 'tco;
                    }
                    Sym(a0sym) if a0sym == "quote" => return Ok(l[1].clone()),
                    Sym(a0sym) if a0sym == "quasiquote" => {
                        live_ast = quasiquote(&l[1]);
                        ast = &live_ast;
                        continue 'tco;
                    }
                    Sym(a0sym) if a0sym == "defmacro!" => {
                        let (a1, a2) = (&l[1], &l[2]);
                        let r = eval(a2, env)?;
                        match r {
                            MalFunc(f) => {
                                return env_set(
                                    env,
                                    a1,
                                    MalFunc(FuncStruct {
                                        is_macro: true,
                                        ..f.clone()
                                    }),
                                )
                            }
                            _ => return error("set_macro on non-function"),
                        }
                    }
                    Sym(a0sym) if a0sym == "do" => {
                        for i in 1..l.len() - 1 {
                            let _ = eval(&l[i], env)?;
                        }
                        live_ast = l.last().unwrap_or(&Nil).clone();
                        ast = &live_ast;
                        continue 'tco;
                    }
                    Sym(a0sym) if a0sym == "if" => {
                        let cond = eval(&l[1], env)?;
                        match cond {
                            Bool(false) | Nil if l.len() >= 4 => {
                                live_ast = l[3].clone();
                                ast = &live_ast;
                                continue 'tco;
                            }
                            Bool(false) | Nil => return Ok(Nil),
                            _ if l.len() >= 3 => {
                                live_ast = l[2].clone();
                                ast = &live_ast;
                                continue 'tco;
                            }
                            _ => return Ok(Nil),
                        }
                    }
                    Sym(a0sym) if a0sym == "fn*" => {
                        let (a1, a2) = (l[1].clone(), l[2].clone());
                        return Ok(MalFunc(FuncStruct {
                            ast: Rc::new(a2),
                            env: env.clone(),
                            params: Rc::new(a1),
                            is_macro: false,
                            meta: Rc::new(Nil),
                        }));
                    }
                    _ => match eval(a0, env)? {
                        f @ MalFunc(FuncStruct { is_macro: true, .. }) => {
                            let new_ast = f.apply(l[1..].to_vec())?;
                            live_ast = new_ast;
                            ast = &live_ast;
                            continue 'tco;
                        }
                        f @ Func(_, _) => {
                            let mut args: MalArgs = vec![];
                            for i in 1..l.len() {
                                args.push(eval(&l[i], env)?);
                            }
                            return f.apply(args);
                        }
                        MalFunc(FuncStruct {
                            ast: mast,
                            env: menv,
                            params: mparams,
                            ..
                        }) => {
                            let mut args: MalArgs = vec![];
                            for i in 1..l.len() {
                                args.push(eval(&l[i], env)?);
                            }
                            live_env = env_bind(menv.clone(), &mparams, args)?;
                            env = &live_env;
                            live_ast = (*mast).clone();
                            ast = &live_ast;
                            continue 'tco;
                        }
                        _ => return error("attempt to call non-function"),
                    },
                }
            }
            _ => return Ok(ast.clone()),
        };
    } // end 'tco loop
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

fn re(str: &str, env: &Env) {
    if let Ok(ast) = read(str) {
        if eval(&ast, env).is_ok() {
            return;
        }
    }
    panic!("error during startup");
}

thread_local! {
    static REPL_ENV: Env = env_new(None);
}

fn main() {
    REPL_ENV.with(|repl_env| {
    let mut args = std::env::args();
    let arg1 = args.nth(1);

    // `()` can be used when no completer is required

    // core.rs: defined using rust
    env_sets(repl_env, "eval", types::func(|a| REPL_ENV.with(|e| eval(&a[0], e))));
    for (k, v) in core::ns() {
        env_sets(repl_env, k, v);
    }
    env_sets(repl_env, "*ARGV*", list(args.map(Str).collect()));

    // core.mal: defined using the language itself
    re("(def! not (fn* (a) (if a false true)))", repl_env);
    re(
        "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))",
        repl_env,
    );
    re("(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))",
        repl_env);

    if let Some(f) = arg1 {
        // Invoked with arguments
        re(&format!("(load-file \"{}\")", f), repl_env);
        std::process::exit(0);
    }

    // main repl loop
    while let Some(ref line) = readline::readline("user> ") {
        if !line.is_empty() {
            match rep(line, repl_env) {
                Ok(ref out) => println!("{}", out),
                Err(ref e) => println!("Error: {}", e.pr_str(true)),
            }
        }
    }
    println!();
  })
}
