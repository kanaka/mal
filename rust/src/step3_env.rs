// support precompiled regexes in reader.rs
#![feature(phase)]
#[phase(plugin)]
extern crate regex_macros;
extern crate regex;

use std::rc::Rc;

use types::{MalVal,MalRet,Int,Sym,List,Vector,Func};
use env::{Env,env_new,env_set,env_get};
mod readline;
mod types;
mod reader;
mod printer;
mod env;

// read
fn read(str: String) -> MalRet {
    reader::read_str(str)
}

// eval
fn eval_ast(ast: MalVal, env: Env) -> MalRet {
    let ast2 = ast.clone();
    match *ast2 {
    //match *ast {
        Sym(ref sym) => {
            env_get(env.clone(), sym.clone())
        },
        List(ref a) => {
            let mut ast_vec : Vec<MalVal> = vec![];
            for mv in a.iter() {
                let mv2 = mv.clone();
                match eval(mv2, env.clone()) {
                    Ok(mv) => { ast_vec.push(mv); },
                    Err(e) => { return Err(e); },
                }
            }
            Ok(Rc::new(List(ast_vec)))
        },
        _ => {
            Ok(ast)
        }
    }
}

fn eval(ast: MalVal, env: Env) -> MalRet {
    let ast2 = ast.clone();
    match *ast2 {
        List(_) => (),  // continue
        _ => return eval_ast(ast2, env),
    }

    // apply list
    match *ast2 {
        List(ref args) => {
            let ref a0 = *args[0];
            match *a0 {
                Sym(ref a0sym) => {
                    match a0sym.as_slice() {
                        "def!" => {
                            let a1 = (*args)[1].clone();
                            let a2 = (*args)[2].clone();
                            let res = eval(a2, env.clone());
                            match res {
                                Ok(r) => {
                                    match *a1 {
                                        Sym(ref s) => {
                                            env_set(&env.clone(), s.clone(), r.clone());
                                            return Ok(r);
                                        },
                                        _ => {
                                            return Err("def! of non-symbol".to_string())
                                        }
                                    }
                                },
                                Err(e) => return Err(e),
                            }
                        },
                        "let*" => {
                            let let_env = env_new(Some(env.clone()));
                            let a1 = (*args)[1].clone();
                            let a2 = (*args)[2].clone();
                            match *a1 {
                                List(ref binds) | Vector(ref binds) => {
                                    let mut it = binds.iter();
                                    while it.len() >= 2 {
                                        let b = it.next().unwrap();
                                        let exp = it.next().unwrap();
                                        match **b {
                                            Sym(ref bstr) => {
                                                match eval(exp.clone(), let_env.clone()) {
                                                    Ok(r) => {
                                                        env_set(&let_env, bstr.clone(), r);
                                                    },
                                                    Err(e) => {
                                                        return Err(e);
                                                    },
                                                }
                                            },
                                            _ => {
                                                return Err("let* with non-symbol binding".to_string());
                                            },
                                        }
                                    }
                                },
                                _ => return Err("let* with non-list bindings".to_string()),
                            }
                            return eval(a2, let_env.clone());
                        },
                        _ => ()
                    }
                }
                _ => (),
            }
            // function call
            return match eval_ast(ast, env) {
                Err(e) => Err(e),
                Ok(el) => {
                    match *el {
                        List(ref args) => {
                            // TODO: make this work
                            //match args.as_slice() {
                            //    [&Func(f), rest..] => {
                            //        (*f)(rest.to_vec())
                            //    },
                            //    _ => Err("attempt to call non-function".to_string()),
                            //}
                            let args2 = args.clone();
                            match *args2[0] {
                                Func(f) => f(args.slice(1,args.len()).to_vec()),
                                _ => Err("attempt to call non-function".to_string()),
                            }
                        }
                        _ => Err("Invalid apply".to_string()),
                    }
                }
            }
        }
        _ => Err("Expected list".to_string()),
    }
}

// print
fn print(exp: MalVal) -> String {
    exp.pr_str(true)
}

fn rep(str: String, env: Env) -> Result<String,String> {
    match read(str) {
        Err(e) => Err(e),
        Ok(ast) => {
            //println!("read: {}", ast);
            match eval(ast, env) {
                Err(e)  => Err(e),
                Ok(exp) => Ok(print(exp)),
            }
        }
    }
}

fn int_op(f: |i:int,j:int|-> int, a:Vec<MalVal>) -> MalRet {
    match *a[0] {
        Int(a0) => match *a[1] {
            Int(a1) => Ok(Rc::new(Int(f(a0,a1)))),
            _ => Err("second arg must be an int".to_string()),
        },
        _ => Err("first arg must be an int".to_string()),
    }
}
fn add(a:Vec<MalVal>) -> MalRet { int_op(|i,j| { i+j }, a) }
fn sub(a:Vec<MalVal>) -> MalRet { int_op(|i,j| { i-j }, a) }
fn mul(a:Vec<MalVal>) -> MalRet { int_op(|i,j| { i*j }, a) }
fn div(a:Vec<MalVal>) -> MalRet { int_op(|i,j| { i/j }, a) }

fn main() {
    let repl_env = env_new(None);
    env_set(&repl_env, "+".to_string(), Rc::new(Func(add)));
    env_set(&repl_env, "-".to_string(), Rc::new(Func(sub)));
    env_set(&repl_env, "*".to_string(), Rc::new(Func(mul)));
    env_set(&repl_env, "/".to_string(), Rc::new(Func(div)));

    loop {
        let line = readline::mal_readline("user> ");
        match line { None => break, _ => () }
        match rep(line.unwrap(), repl_env.clone()) {
            Ok(str)  => println!("{}", str),
            Err(str) => println!("Error: {}", str),
        }
    }
}
