// support precompiled regexes in reader.rs
#![feature(phase)]
#[phase(plugin)]
extern crate regex_macros;
extern crate regex;

use types::{MalVal,MalRet,
            Nil,False,Sym,List,Vector,
            _nil,list,malfunc};
use env::{Env,env_new,env_set,env_get};
mod readline;
mod types;
mod reader;
mod printer;
mod env;
mod core;

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
            Ok(list(ast_vec))
        },
        _ => {
            Ok(ast)
        }
    }
}

fn eval(ast: MalVal, env: Env) -> MalRet {
    //println!("eval: {}, {}", ast, env.borrow());
    //println!("eval: {}", ast);
    let ast2 = ast.clone();
    match *ast2 {
        List(_) => (),  // continue
        _ => return eval_ast(ast2, env),
    }

    // apply list
    match *ast2 {
        List(ref args) => {
            if args.len() == 0 { 
                return Ok(ast);
            }
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
                        "do" => {
                            let el = list(args.slice(1,args.len()).to_vec());
                            match eval_ast(el, env.clone()) {
                                Err(e) => return Err(e),
                                Ok(el) => {
                                    match *el {
                                        List(ref lst) => {
                                            let ref last = lst[lst.len()-1];
                                            return Ok(last.clone());
                                        }
                                        _ => (),
                                    }
                                },
                            }
                        },
                        "if" => {
                            let a1 = (*args)[1].clone();
                            let cond = eval(a1, env.clone());
                            if cond.is_err() { return cond; }
                            match *cond.unwrap() {
                                False | Nil => {
                                    if args.len() >= 4 {
                                        let a3 = (*args)[3].clone();
                                        return eval(a3, env.clone());
                                    } else {
                                        return Ok(_nil());
                                    }
                                },
                                _ => {
                                    let a2 = (*args)[2].clone();
                                    return eval(a2, env.clone());
                                },
                            }
                        },
                        "fn*" => {
                            let a1 = (*args)[1].clone();
                            let a2 = (*args)[2].clone();
                            return Ok(malfunc(eval, a2, env.clone(), a1));
                        },
                        _ => ()
                    }
                }
                _ => (),
            }
            // function call
            return match eval_ast(ast, env.clone()) {
                Err(e) => Err(e),
                Ok(el) => {
                    match *el {
                        List(ref args) => {
                            let ref f = args.clone()[0];
                            f.apply(args.slice(1,args.len()).to_vec())
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

fn main() {
    let repl_env = env_new(None);
    for (k, v) in core::ns().into_iter() { env_set(&repl_env, k, v); }

    let _ = rep("(def! not (fn* (a) (if a false true)))".to_string(),
                repl_env.clone());

    loop {
        let line = readline::mal_readline("user> ");
        match line { None => break, _ => () }
        match rep(line.unwrap(), repl_env.clone()) {
            Ok(str)  => println!("{}", str),
            Err(str) => println!("Error: {}", str),
        }
    }
}
