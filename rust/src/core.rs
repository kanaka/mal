#![allow(dead_code)]

use std::rc::Rc;
use std::collections::HashMap;

use types::{MalVal,MalRet,Func,True,False,Int,Strn,List,Nil};
use printer;

// General functions

fn equal_q(a:Vec<MalVal>) -> MalRet {
    if a.len() != 2 {
        return Err("Wrong arity to equal? call".to_string());
    }
    match a[0] == a[1] {
        true => Ok(Rc::new(True)),
        false => Ok(Rc::new(False)),
    }
}

// String routines
fn pr_str(a:Vec<MalVal>) -> MalRet {
    Ok(Rc::new(Strn(printer::pr_list(&a, true, "", "", " "))))
}

fn str(a:Vec<MalVal>) -> MalRet {
    Ok(Rc::new(Strn(printer::pr_list(&a, false, "", "", ""))))
}

fn prn(a:Vec<MalVal>) -> MalRet {
    println!("{}", printer::pr_list(&a, true, "", "", " "))
    Ok(Rc::new(Nil))
}

fn println(a:Vec<MalVal>) -> MalRet {
    println!("{}", printer::pr_list(&a, false, "", "", " "))
    Ok(Rc::new(Nil))
}

// Numeric functions
fn int_op(f: |i:int,j:int|-> int, a:Vec<MalVal>) -> MalRet {
    match *a[0] {
        Int(a0) => match *a[1] {
            Int(a1) => Ok(Rc::new(Int(f(a0,a1)))),
            _ => Err("second arg must be an int".to_string()),
        },
        _ => Err("first arg must be an int".to_string()),
    }
}

fn bool_op(f: |i:int,j:int|-> bool, a:Vec<MalVal>) -> MalRet {
    match *a[0] {
        Int(a0) => match *a[1] {
            Int(a1) => {
                match f(a0,a1) {
                    true => Ok(Rc::new(True)),
                    false => Ok(Rc::new(False)), 
                }
                //Ok(Rc::new(Int(f(a0,a1)))),
            },
            _ => Err("second arg must be an int".to_string()),
        },
        _ => Err("first arg must be an int".to_string()),
    }
}

pub fn add(a:Vec<MalVal>) -> MalRet { int_op(|i,j| { i+j }, a) }
pub fn sub(a:Vec<MalVal>) -> MalRet { int_op(|i,j| { i-j }, a) }
pub fn mul(a:Vec<MalVal>) -> MalRet { int_op(|i,j| { i*j }, a) }
pub fn div(a:Vec<MalVal>) -> MalRet { int_op(|i,j| { i/j }, a) }

pub fn lt (a:Vec<MalVal>) -> MalRet { bool_op(|i,j| { i<j }, a) }
pub fn lte(a:Vec<MalVal>) -> MalRet { bool_op(|i,j| { i<=j }, a) }
pub fn gt (a:Vec<MalVal>) -> MalRet { bool_op(|i,j| { i>j }, a) }
pub fn gte(a:Vec<MalVal>) -> MalRet { bool_op(|i,j| { i>=j }, a) }


// Sequence functions
pub fn list(a:Vec<MalVal>) -> MalRet {
    Ok(Rc::new(List(a)))
}

pub fn list_q(a:Vec<MalVal>) -> MalRet {
    if a.len() != 1 {
        return Err("Wrong arity to list? call".to_string());
    }
    match *a[0].clone() {
        List(_) => Ok(Rc::new(True)),
        _ => Ok(Rc::new(False)),
    }
}

pub fn count(a:Vec<MalVal>) -> MalRet {
    if a.len() != 1 {
        return Err("Wrong arity to count call".to_string());
    }
    match *a[0].clone() {
        List(ref lst) => {
            Ok(Rc::new(Int(lst.len().to_int().unwrap())))
        },
        _ => Err("count called on non-sequence".to_string()),
    }
}

pub fn empty_q(a:Vec<MalVal>) -> MalRet {
    if a.len() != 1 {
        return Err("Wrong arity to empty? call".to_string());
    }
    match *a[0].clone() {
        List(ref lst) => {
            match lst.len() {
                0 => Ok(Rc::new(True)),
                _ => Ok(Rc::new(False)),
            }
        },
        _ => Err("empty? called on non-sequence".to_string()),
    }
}



pub fn ns() -> HashMap<String,MalVal> {
    let mut ns: HashMap<String,MalVal> = HashMap::new();;

    ns.insert("=".to_string(), Rc::new(Func(equal_q)));

    ns.insert("pr-str".to_string(), Rc::new(Func(pr_str)));
    ns.insert("str".to_string(), Rc::new(Func(str)));
    ns.insert("prn".to_string(), Rc::new(Func(prn)));
    ns.insert("println".to_string(), Rc::new(Func(println)));

    ns.insert("<".to_string(),  Rc::new(Func(lt)));
    ns.insert("<=".to_string(), Rc::new(Func(lte)));
    ns.insert(">".to_string(),  Rc::new(Func(gt)));
    ns.insert(">=".to_string(), Rc::new(Func(gte)));
    ns.insert("+".to_string(), Rc::new(Func(add)));
    ns.insert("-".to_string(), Rc::new(Func(sub)));
    ns.insert("*".to_string(), Rc::new(Func(mul)));
    ns.insert("/".to_string(), Rc::new(Func(div)));

    ns.insert("list".to_string(), Rc::new(Func(list)));
    ns.insert("list?".to_string(), Rc::new(Func(list_q)));
    ns.insert("empty?".to_string(), Rc::new(Func(empty_q)));
    ns.insert("count".to_string(), Rc::new(Func(count)));

    return ns;
}
