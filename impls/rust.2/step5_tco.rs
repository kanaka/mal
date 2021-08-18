extern crate rustyline;

use rustyline::error::ReadlineError;
use rustyline::Editor;

mod core;
mod env;
mod printer;
mod reader;
mod types;

use std::rc::Rc;
use types::MalValue::MalList;
use types::{MalError, MalResult, MalValue};

fn read(input: String) -> Result<Option<MalValue>, MalError> {
    return reader::Reader::read_str(input.to_string());
}
fn eval(mut ast: MalValue, mut env: Rc<env::Environment>) -> MalResult {
    let result: MalResult;

    'tco: loop {
        result = match ast.clone() {
            MalValue::MalList(list) => {
                if list.len() == 0 {
                    return Ok(ast.clone());
                }
                let first_element = &list[0];
                match first_element {
                    MalValue::MalSymbol(ref s) if s == "def!" => {
                        return env.set(list[1].clone(), eval(list[2].clone(), env.clone())?);
                    }
                    MalValue::MalSymbol(ref s) if s == "let*" => {
                        assert_eq!(3, list.len());
                        let bindings = list[1].clone();
                        let expr = list[2].clone();

                        let new_env = Rc::new(env::Environment::new(Some(env.clone()), None, None));

                        if let Some(binding_list) = bindings.as_vec() {
                            assert!(binding_list.len() % 2 == 0);
                            let mut key = binding_list.first().unwrap();
                            for i in 0..binding_list.len() {
                                if i % 2 == 0 {
                                    key = binding_list.get(i).unwrap();
                                } else {
                                    new_env.set(
                                        key.clone(),
                                        eval(
                                            binding_list.get(i).unwrap().clone(),
                                            new_env.clone(),
                                        )?,
                                    )?;
                                }
                            }

                            ast = expr;
                            env = new_env;
                            continue 'tco;
                        } else {
                            return Err(MalError::EvalError(String::from("Missing bindings!")));
                        }
                    }
                    MalValue::MalSymbol(ref s) if s == "fn*" => {
                        return Ok(MalValue::MalFunc {
                            eval: eval,
                            ast: Rc::new(list[2].clone()),
                            env: env.clone(),
                            params: Rc::new(list[1].clone()),
                        });
                    }
                    MalValue::MalSymbol(ref s) if s == "do" => {
                        for token in &list[1..list.len() - 1] {
                            eval_ast(token.clone(), env.clone())?;
                        }

                        ast = list.last().unwrap().clone();
                        continue 'tco;
                    }
                    MalValue::MalSymbol(ref s) if s == "if" => {
                        assert!(list.len() >= 3);
                        let condition = eval(list[1].clone(), env.clone())?;
                        let true_expr = list[2].clone();
                        if condition.is_truthy() {
                            ast = true_expr.clone();
                            continue 'tco;
                        } else if list.len() == 4 {
                            ast = list[3].clone();
                            continue 'tco;
                        }
                        return Ok(MalValue::MalNil);
                    }
                    _ => match eval_ast(ast.clone(), env.clone())? {
                        MalList(list) => match list[0] {
                            MalValue::MalFunction(f, _) => return f(list[1..].to_vec()),
                            MalValue::MalFunc {
                                ast: ref fn_ast,
                                env: ref fn_env,
                                ref params,
                                ..
                            } => {
                                let a = &**fn_ast;
                                ast = a.clone();
                                env = Rc::new(env::Environment::new(
                                    Some(fn_env.clone()),
                                    params.as_vec(),
                                    Some(list[1..].to_vec()),
                                ));
                                continue 'tco;
                            }
                            _ => {
                                return Err(MalError::EvalError(
                                    "Attempt to call non-function".to_string(),
                                ))
                            }
                        },
                        _ => {
                            return Err(MalError::EvalError(String::from("Expected a list!")));
                        }
                    },
                }
            }
            _ => {
                return eval_ast(ast, env);
            }
        };

        break;
    }

    return result;
}

fn eval_ast(ast: MalValue, env: Rc<env::Environment>) -> Result<MalValue, MalError> {
    match ast.clone() {
        MalValue::MalSymbol(s) => {
            if let Ok(func) = env.get(ast.clone()) {
                return Ok(func);
            }
            return Err(types::MalError::EvalError(format!(
                "Symbol '{}' not found",
                s
            )));
        }
        MalValue::MalList(list) => {
            let mut result = Vec::<types::MalValue>::new();

            for token in list {
                result.push(eval(token, env.clone())?);
            }

            return Ok(types::MalValue::MalList(result));
        }
        MalValue::MalVector(vector) => {
            let mut result = Vec::<types::MalValue>::new();

            for token in vector {
                result.push(eval(token, env.clone())?);
            }

            return Ok(types::MalValue::MalVector(result));
        }
        MalValue::MalHashmap(keys, values) => {
            let mut result = Vec::<types::MalValue>::new();

            for token in values {
                result.push(eval(token, env.clone())?);
            }

            return Ok(types::MalValue::MalHashmap(keys, result));
        }
        _ => {
            return Ok(ast);
        }
    }
}

fn print(input: MalValue) -> String {
    return crate::printer::pr_str(input, true);
}

fn rep(input: String, env: &Rc<env::Environment>) -> String {
    let ast = read(input);
    match ast {
        Err(e) => {
            return format!("Error! {:?}", e);
        }
        Ok(v) => match v {
            None => return String::default(),
            Some(a) => match eval(a, env.clone()) {
                Ok(result) => {
                    return print(result);
                }
                Err(e) => {
                    return format!("Error! {:?}", e);
                }
            },
        },
    }
}

fn main() {
    let mut rl = Editor::<()>::new();
    if rl.load_history(".mal-history").is_err() {
        println!("No previous history.");
    }

    let env = Rc::new(env::Environment::new(None, None, None));

    for (symbol, func) in crate::core::ns() {
        if let Err(e) = env.set(MalValue::MalSymbol(symbol.to_string()), func) {
            panic!("Failed add core item {}. {:#?}", symbol, e)
        }
    }

    rep("(def! not (fn* (a) (if a false true)))".to_string(), &env);
    rep(
        "(def! sum2 (fn* (n acc) (if (= n 0) acc (sum2 (- n 1) (+ n acc)))))".to_string(),
        &env,
    );

    loop {
        let readline = rl.readline("user> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                let result = rep(line.trim_end().to_string(), &env);
                print!("{}", result);
                println!();
            }
            Err(ReadlineError::Interrupted) => break,
            Err(ReadlineError::Eof) => break,
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
}
