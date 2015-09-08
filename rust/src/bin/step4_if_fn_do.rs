extern crate mal;

use std::collections::HashMap;

use mal::types::{MalVal, MalRet, MalError, err_str};
use mal::types::{symbol, _nil, list, vector, hash_map, malfunc};
use mal::types::MalError::{ErrString, ErrMalVal};
use mal::types::MalType::{Nil, False, Sym, List, Vector, Hash_Map};
use mal::{readline, reader, core};
use mal::env::{env_set, env_get, env_new, Env};


// read
fn read(str: String) -> MalRet {
    reader::read_str(str)
}

// eval
fn eval_ast(ast: MalVal, env: Env) -> MalRet {
    match *ast {
        Sym(_) => env_get(&env, &ast),
        List(ref a,_) | Vector(ref a,_) => {
            let mut ast_vec : Vec<MalVal> = vec![];
            for mv in a.iter() {
                let mv2 = mv.clone();
                ast_vec.push(try!(eval(mv2, env.clone())));
            }
            Ok(match *ast { List(_,_) => list(ast_vec),
                            _         => vector(ast_vec) })
        }
        Hash_Map(ref hm,_) => {
            let mut new_hm: HashMap<String,MalVal> = HashMap::new();
            for (key, value) in hm.iter() {
                new_hm.insert(key.to_string(),
                              try!(eval(value.clone(), env.clone())));
            }
            Ok(hash_map(new_hm))
        }
        _ => Ok(ast.clone()),
    }
}

fn eval(ast: MalVal, env: Env) -> MalRet {
    //println!("eval: {}, {}", ast, env.borrow());
    //println!("eval: {}", ast);
    match *ast {
        List(_,_) => (),  // continue
        _ => return eval_ast(ast, env),
    }

    // apply list
    match *ast {
        List(_,_) => (),  // continue
        _ => return Ok(ast),
    }

    let tmp = ast;
    let (args, a0sym) = match *tmp {
        List(ref args,_) => {
            if args.len() == 0 {
                return Ok(tmp.clone());
            }
            let ref a0 = *args[0];
            match *a0 {
                Sym(ref a0sym) => (args, &a0sym[..]),
                _ => (args, "__<fn*>__"),
            }
        },
        _ => return err_str("Expected list"),
    };

    match a0sym {
        "def!" => {
            let a1 = (*args)[1].clone();
            let a2 = (*args)[2].clone();
            let r = try!(eval(a2, env.clone()));
            match *a1 {
                Sym(_) => {
                    env_set(&env.clone(), a1, r.clone());
                    return Ok(r);
                },
                _ => return err_str("def! of non-symbol"),
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
                                let r = try!(eval(exp.clone(), let_env.clone()));
                                env_set(&let_env, b.clone(), r);
                            },
                            _ => return err_str("let* with non-symbol binding"),
                        }
                    }
                },
                _ => return err_str("let* with non-list bindings"),
            }
            return eval(a2, let_env.clone());
        },
        "do" => {
            let el = list(args[1..].to_vec());
            match *try!(eval_ast(el, env.clone())) {
                List(ref lst,_) => {
                    let ref last = lst[lst.len()-1];
                    return Ok(last.clone());
                }
                _ => return err_str("invalid do call"),
            }
        },
        "if" => {
            let a1 = (*args)[1].clone();
            let c = try!(eval(a1, env.clone()));
            match *c {
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
            let a1 = args[1].clone();
            let a2 = args[2].clone();
            return Ok(malfunc(eval, a2, env, a1, _nil()));
        },
        _ => { // function call
            let el = try!(eval_ast(tmp.clone(), env.clone()));
            let args = match *el {
                List(ref args,_) => args,
                _ => return err_str("Invalid apply"),
            };
            let ref f = args.clone()[0];
            f.apply(args[1..].to_vec())
        },
    }
}

// print
fn print(exp: MalVal) -> String {
    exp.pr_str(true)
}

fn rep(str: &str, env: Env) -> Result<String,MalError> {
    let ast = try!(read(str.to_string()));
    //println!("read: {}", ast);
    let exp = try!(eval(ast, env));
    Ok(print(exp))
}

fn main() {
    // core.rs: defined using rust
    let repl_env = env_new(None);
    for (k, v) in core::ns().into_iter() {
        env_set(&repl_env, symbol(&k), v);
    }

    // core.mal: defined using the language itself
    let _ = rep("(def! not (fn* (a) (if a false true)))", repl_env.clone());

    // repl loop
    loop {
        let line = readline::mal_readline("user> ");
        match line { None => break, _ => () }
        match rep(&line.unwrap(), repl_env.clone()) {
            Ok(str)  => println!("{}", str),
            Err(ErrMalVal(_)) => (),  // Blank line
            Err(ErrString(s)) => println!("Error: {}", s),
        }
    }
}
