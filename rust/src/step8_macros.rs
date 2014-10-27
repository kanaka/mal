// support precompiled regexes in reader.rs
#![feature(phase)]
#[phase(plugin)]
extern crate regex_macros;
extern crate regex;

use std::os;

use types::{MalVal,MalRet,MalFunc,
            Nil,False,Sym,List,Vector,Func,
            _nil,symbol,string,list,malfunc,malfuncd};
use env::{Env,env_new,env_bind,env_root,env_find,env_set,env_get};
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
fn is_pair(x: MalVal) -> bool {
    match *x {
        List(ref lst) => lst.len() > 0,
        _ => false,
    }
}

fn quasiquote(ast: MalVal) -> MalVal {
    if !is_pair(ast.clone()) {
        return list(vec![symbol("quote"), ast])
    }

    match *ast.clone() {
        List(ref args) => {
            let ref a0 = args[0];
            match **a0 {
                Sym(ref s) => {
                    if s.to_string() == "unquote".to_string() {
                        let ref a1 = args[1];
                        return a1.clone();
                    }
                },
                _ => (),
            }
            if is_pair(a0.clone()) {
                match **a0 {
                    List(ref a0args) => {
                        let a00 = a0args[0].clone();
                        match *a00 {
                            Sym(ref s) => {
                                if s.to_string() == "splice-unquote".to_string() {
                                    return list(vec![symbol("concat"),
                                                     a0args[1].clone(),
                                                     quasiquote(list(args.slice(1,args.len()).to_vec()))])
                                }
                            },
                            _ => (),
                        }
                    },
                    _ => (),
                }
            }
            let rest = list(args.slice(1,args.len()).to_vec());
            return list(vec![symbol("cons"),
                             quasiquote(a0.clone()),
                             quasiquote(rest)])
        },
        _ => _nil(), // should never reach
    }
}

fn is_macro_call(ast: MalVal, env: Env) -> bool {
    match *ast {
        List(ref lst) => {
            let ref a0 = *lst[0];
            match *a0 {
                Sym(ref a0sym) => {
                    if env_find(env.clone(), a0sym.to_string()).is_some() {
                        match env_get(env, a0sym.to_string()) {
                            Ok(f) => {
                                match *f {
                                    MalFunc(ref mfd) => {
                                        mfd.is_macro
                                    },
                                    _ => false,
                                }
                            },
                            _ => false,
                        }
                    } else {
                        false
                    }
                },
                _ => false,
            }
        },
        _ => false,
    }
}

fn macroexpand(mut ast: MalVal, env: Env) -> MalRet {
    while is_macro_call(ast.clone(), env.clone()) {
        match *ast.clone() {
            List(ref args) => {
                let ref a0 = args[0];
                match **a0 {
                    Sym(ref s) => {
                        match env_get(env.clone(), s.to_string()) {
                            Ok(mf) => {
                                match *mf {
                                    MalFunc(_) => {
                                        match mf.apply(args.slice(1,args.len()).to_vec()) {
                                            Ok(r) => ast = r,
                                            Err(e) => return Err(e),
                                        }
                                    },
                                    _ => break,
                                }
                            },
                            Err(e) => return Err(e),
                        }
                    },
                    _ => break,
                }
            },
            _ => break,
        }
    }
    Ok(ast)
}

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

fn eval(mut ast: MalVal, mut env: Env) -> MalRet {
    'tco: loop {

    //println!("eval: {}, {}", ast, env.borrow());
    //println!("eval: {}", ast);
    let mut ast2 = ast.clone();
    match *ast2 {
        List(_) => (),  // continue
        _ => return eval_ast(ast2, env),
    }

    // apply list
    match macroexpand(ast2, env.clone()) {
        Ok(a) => {
            ast2 = a;
        },
        Err(e) => return Err(e),
    }
    match *ast2 {
        List(_) => (),  // continue
        _ => return Ok(ast2),
    }
    let ast3 = ast2.clone();

    match *ast2 {
        List(ref args) => {
            if args.len() == 0 { 
                return Ok(ast3);
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
                            ast = a2;
                            env = let_env.clone();
                            continue 'tco;
                        },
                        "quote" => {
                            return Ok((*args)[1].clone());
                        },
                        "quasiquote" => {
                            let a1 = (*args)[1].clone();
                            ast = quasiquote(a1);
                            continue 'tco;
                        },
                        "defmacro!" => {
                            let a1 = (*args)[1].clone();
                            let a2 = (*args)[2].clone();
                            match eval(a2, env.clone()) {
                                Ok(r) => {
                                    match *r {
                                        MalFunc(ref mfd) => {
                                            match *a1 {
                                                Sym(ref s) => {
                                                    let mut new_mfd = mfd.clone();
                                                    new_mfd.is_macro = true;
                                                    let mf = malfuncd(new_mfd);
                                                    env_set(&env.clone(), s.clone(), mf.clone());
                                                    return Ok(mf);
                                                },
                                                _ => return Err("def! of non-symbol".to_string()),
                                            }
                                        },
                                        _ => return Err("def! of non-symbol".to_string()),
                                    }
                                },
                                Err(e) => return Err(e),
                            }
                        },
                        "macroexpand" => {
                            let a1 = (*args)[1].clone();
                            return macroexpand(a1, env.clone())
                        },
                        "do" => {
                            let el = list(args.slice(1,args.len()-1).to_vec());
                            match eval_ast(el, env.clone()) {
                                Err(e) => return Err(e),
                                Ok(_) => {
                                    let ref last = args[args.len()-1];
                                    ast = last.clone();
                                    continue 'tco;
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
                                        ast = a3;
                                        env = env.clone();
                                        continue 'tco;
                                    } else {
                                        return Ok(_nil());
                                    }
                                },
                                _ => {
                                    let a2 = (*args)[2].clone();
                                    ast = a2;
                                    env = env.clone();
                                    continue 'tco;
                                },
                            }
                        },
                        "fn*" => {
                            let a1 = (*args)[1].clone();
                            let a2 = (*args)[2].clone();
                            return Ok(malfunc(eval, a2, env.clone(), a1));
                        },
                        "eval" => {
                            let a1 = (*args)[1].clone();
                            match eval(a1, env.clone()) {
                                Ok(exp) => {
                                    ast = exp;
                                    env = env_root(&env);
                                    continue 'tco;
                                },
                                Err(e) => return Err(e),
                            }
                        },
                        _ => ()
                    }
                }
                _ => (),
            }
            // function call
            /*
            if is_macro_call(ast3.clone(), env.clone()) {
                println!("macro call");
            }
            */
            return match eval_ast(ast3, env.clone()) {
                Err(e) => Err(e),
                Ok(el) => {
                    match *el {
                        List(ref args) => {
                            let args2 = args.clone();
                            match *args2[0] {
                                Func(f) => f(args.slice(1,args.len()).to_vec()),
                                MalFunc(ref mf) => {
                                    let mfc = mf.clone();
                                    let alst = list(args.slice(1,args.len()).to_vec());
                                    let new_env = env_new(Some(mfc.env.clone()));
                                    match env_bind(&new_env, mfc.params, alst) {
                                        Ok(_) => {
                                            ast = mfc.exp;
                                            env = new_env;
                                            continue 'tco;
                                        },
                                        Err(e) => Err(e),
                                    }
                                },
                                _ => Err("attempt to call non-function".to_string()),
                            }
                        }
                        _ => Err("Invalid apply".to_string()),
                    }
                }
            }
        }
        _ => return Err("Expected list".to_string()),
    }

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
    // core.rs: defined using rust
    let repl_env = env_new(None);
    for (k, v) in core::ns().into_iter() { env_set(&repl_env, k, v); }
    // see eval() for definition of "eval"
    env_set(&repl_env, "*ARGV*".to_string(), list(vec![]));

    // core.mal: defined using the language itself
    let _ = rep("(def! not (fn* (a) (if a false true)))".to_string(),
                repl_env.clone());
    let _ = rep("(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \")\")))))".to_string(),
                repl_env.clone());

    // Invoked with command line arguments
    let args = os::args();
    if args.len() > 1 {
        let mv_args = args.slice(2,args.len()).iter()
            .map(|a| string(a.to_string()))
            .collect::<Vec<MalVal>>();
        env_set(&repl_env, "*ARGV*".to_string(), list(mv_args));
        match rep("(load-file \"".to_string() + args[1] + "\")".to_string(),
                  repl_env.clone()) {
            Ok(_) => {
                os::set_exit_status(0);
                return;
            },
            Err(str) => {
                println!("Error: {}", str);
                os::set_exit_status(1);
                return;
            },
        }
    }

    loop {
        let line = readline::mal_readline("user> ");
        match line { None => break, _ => () }
        match rep(line.unwrap(), repl_env.clone()) {
            Ok(str)  => println!("{}", str),
            Err(str) => println!("Error: {}", str),
        }
    }
}
