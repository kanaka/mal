// support precompiled regexes in reader.rs
#![feature(phase)]
#[phase(plugin)]
extern crate regex_macros;
extern crate regex;

use std::collections::HashMap;

use types::{MalVal,MalRet,MalError,ErrString,ErrMalVal,err_str,
            Nil,False,Sym,List,Vector,Hash_Map,
            symbol,_nil,list,vector,hash_map,malfunc};
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
        Sym(_) => {
            env_get(env.clone(), ast)
        },
        List(ref a,_) | Vector(ref a,_) => {
            let mut ast_vec : Vec<MalVal> = vec![];
            for mv in a.iter() {
                let mv2 = mv.clone();
                match eval(mv2, env.clone()) {
                    Ok(mv) => { ast_vec.push(mv); },
                    Err(e) => { return Err(e); },
                }
            }
            Ok(match *ast { List(_,_) => list(ast_vec),
                            _         => vector(ast_vec) })
        },
        Hash_Map(ref hm,_) => {
            let mut new_hm: HashMap<String,MalVal> = HashMap::new();
            for (key, value) in hm.iter() {
                match eval(value.clone(), env.clone()) {
                    Ok(mv) => { new_hm.insert(key.to_string(), mv); },
                    Err(e) => return Err(e),
                }
            }
            Ok(hash_map(new_hm))
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
        List(_,_) => (),  // continue
        _ => return eval_ast(ast2, env),
    }

    // apply list
    match *ast2 {
        List(_,_) => (),  // continue
        _ => return Ok(ast2),
    }

    let (args, a0sym) = match *ast2 {
        List(ref args,_) => {
            if args.len() == 0 { 
                return Ok(ast);
            }
            let ref a0 = *args[0];
            match *a0 {
                Sym(ref a0sym) => (args, a0sym.as_slice()),
                _ => (args, "__<fn*>__"),
            }
        },
        _ => return err_str("Expected list"),
    };

    match a0sym {
        "def!" => {
            let a1 = (*args)[1].clone();
            let a2 = (*args)[2].clone();
            let res = eval(a2, env.clone());
            match res {
                Ok(r) => {
                    match *a1 {
                        Sym(_) => {
                            env_set(&env.clone(), a1.clone(), r.clone());
                            return Ok(r);
                        },
                        _ => {
                            return err_str("def! of non-symbol")
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
                List(ref binds,_) | Vector(ref binds,_) => {
                    let mut it = binds.iter();
                    while it.len() >= 2 {
                        let b = it.next().unwrap();
                        let exp = it.next().unwrap();
                        match **b {
                            Sym(_) => {
                                match eval(exp.clone(), let_env.clone()) {
                                    Ok(r) => {
                                        env_set(&let_env, b.clone(), r);
                                    },
                                    Err(e) => {
                                        return Err(e);
                                    },
                                }
                            },
                            _ => {
                                return err_str("let* with non-symbol binding");
                            },
                        }
                    }
                },
                _ => return err_str("let* with non-list bindings"),
            }
            return eval(a2, let_env.clone());
        },
        "do" => {
            let el = list(args.slice(1,args.len()).to_vec());
            return match eval_ast(el, env.clone()) {
                Err(e) => return Err(e),
                Ok(el) => {
                    match *el {
                        List(ref lst,_) => {
                            let ref last = lst[lst.len()-1];
                            return Ok(last.clone());
                        }
                        _ => return err_str("invalid do call"),
                    }
                },
            };
        },
        "if" => {
            let a1 = (*args)[1].clone();
            let cond = eval(a1, env.clone());
            match cond {
                Err(e) => return Err(e),
                Ok(c) => match *c {
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
            }
        },
        "fn*" => {
            let a1 = (*args)[1].clone();
            let a2 = (*args)[2].clone();
            return Ok(malfunc(eval, a2, env.clone(), a1, _nil()));
        },
        _ => { // function call
            return match eval_ast(ast, env.clone()) {
                Err(e) => Err(e),
                Ok(el) => {
                    let args = match *el {
                        List(ref args,_) => args,
                        _ => return err_str("Invalid apply"),
                    };
                    let ref f = args.clone()[0];
                    f.apply(args.slice(1,args.len()).to_vec())
                }
            };
        },
    }
}

// print
fn print(exp: MalVal) -> String {
    exp.pr_str(true)
}

fn rep(str: &str, env: Env) -> Result<String,MalError> {
    match read(str.to_string()) {
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
    // core.rs: defined using rust
    let repl_env = env_new(None);
    for (k, v) in core::ns().into_iter() {
        env_set(&repl_env, symbol(k.as_slice()), v);
    }

    // core.mal: defined using the language itself
    let _ = rep("(def! not (fn* (a) (if a false true)))", repl_env.clone());

    loop {
        let line = readline::mal_readline("user> ");
        match line { None => break, _ => () }
        match rep(line.unwrap().as_slice(), repl_env.clone()) {
            Ok(str)  => println!("{}", str),
            Err(ErrMalVal(_)) => (),  // Blank line
            Err(ErrString(s)) => println!("Error: {}", s),
        }
    }
}
