#![allow(dead_code)]

extern crate time;
use std::collections::HashMap;
use std::io::File;

use types::{MalVal,MalRet,Int,Strn,List,
            _nil,_true,_false,_int,string,list,func};
use reader;
use printer;

// General functions

fn equal_q(a:Vec<MalVal>) -> MalRet {
    if a.len() != 2 {
        return Err("Wrong arity to equal? call".to_string());
    }
    match a[0] == a[1] {
        true => Ok(_true()),
        false => Ok(_false()),
    }
}

// String routines
fn pr_str(a:Vec<MalVal>) -> MalRet {
    Ok(string(printer::pr_list(&a, true, "", "", " ")))
}

fn str(a:Vec<MalVal>) -> MalRet {
    Ok(string(printer::pr_list(&a, false, "", "", "")))
}

fn prn(a:Vec<MalVal>) -> MalRet {
    println!("{}", printer::pr_list(&a, true, "", "", " "))
    Ok(_nil())
}

fn println(a:Vec<MalVal>) -> MalRet {
    println!("{}", printer::pr_list(&a, false, "", "", " "))
    Ok(_nil())
}

fn read_string(a:Vec<MalVal>) -> MalRet {
    match *a[0] {
        Strn(ref a0) => reader::read_str(a0.to_string()),
        _ => Err("read_string called with non-string".to_string()),
    }
}

fn slurp(a:Vec<MalVal>) -> MalRet {
    match *a[0] {
        Strn(ref a0) => {
            match File::open(&Path::new(a0.as_slice())).read_to_string() {
                Ok(s) => Ok(string(s)),
                Err(e) => Err(e.to_string()),
            }
        },
        _ => Err("slurp called with non-string".to_string()),
    }
}


// Numeric functions
fn int_op(f: |i:int,j:int|-> int, a:Vec<MalVal>) -> MalRet {
    match *a[0] {
        Int(a0) => match *a[1] {
            Int(a1) => Ok(_int(f(a0,a1))),
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
                    true => Ok(_true()),
                    false => Ok(_false()), 
                }
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

#[allow(unused_variable)]
pub fn time_ms(a:Vec<MalVal>) -> MalRet {
    //let x = time::now();
    let now = time::get_time();
    let now_ms = (now.sec * 1000).to_int().unwrap() + (now.nsec.to_int().unwrap() / 1000000);
    Ok(_int(now_ms))
}


// Sequence functions
pub fn _list(a:Vec<MalVal>) -> MalRet { Ok(list(a)) }

pub fn list_q(a:Vec<MalVal>) -> MalRet {
    if a.len() != 1 {
        return Err("Wrong arity to list? call".to_string());
    }
    match *a[0].clone() {
        List(_) => Ok(_true()),
        _ => Ok(_false()),
    }
}

pub fn cons(a:Vec<MalVal>) -> MalRet {
    match *a[1] {
        List(ref v) => {
            let mut new_v = v.clone();
            new_v.insert(0, a[0].clone());
            Ok(list(new_v))
        },
        _ => Err("Second arg to cons not a sequence".to_string()),
    }
}

pub fn concat(a:Vec<MalVal>) -> MalRet {
    let mut new_v:Vec<MalVal> = vec![];
    for lst in a.iter() {
        match **lst {
            List(ref l) => {
                new_v.push_all(l.as_slice());
            },
            _ => return Err("concat called with non-sequence".to_string()),
        }
    }
    Ok(list(new_v))
}

pub fn nth(a:Vec<MalVal>) -> MalRet {
    if a.len() != 2 {
        return Err("Wrong arity to nth call".to_string());
    }
    let a0 = a[0].clone();
    let a1 = a[1].clone();
    let seq = match *a0 {
        List(ref v) => v,
        _ => return Err("nth called with non-sequence".to_string()),
    };
    let idx = match *a1 {
        Int(i) => {
            match i.to_uint() {
                Some(ui) => ui,
                None => return Ok(_nil()),
            }
        },
        _ => return Err("nth called with non-integer index".to_string()),
    };
    if idx >= seq.len() {
        Ok(_nil())
    } else {
        Ok(seq[idx].clone())
    }
}

pub fn first(a:Vec<MalVal>) -> MalRet {
    if a.len() != 1 {
        return Err("Wrong arity to first call".to_string());
    }
    let a0 = a[0].clone();
    let seq = match *a0 {
        List(ref v) => v,
        _ => return Err("first called with non-sequence".to_string()),
    };
    if seq.len() == 0 {
        Ok(_nil())
    } else {
        Ok(seq[0].clone())
    }
}

pub fn rest(a:Vec<MalVal>) -> MalRet {
    if a.len() != 1 {
        return Err("Wrong arity to rest call".to_string());
    }
    let a0 = a[0].clone();
    let seq = match *a0 {
        List(ref v) => v,
        _ => return Err("rest called with non-sequence".to_string()),
    };
    if seq.len() == 0 {
        Ok(list(vec![]))
    } else {
        Ok(list(seq.slice(1,seq.len()).to_vec()))
    }
}

pub fn count(a:Vec<MalVal>) -> MalRet {
    if a.len() != 1 {
        return Err("Wrong arity to count call".to_string());
    }
    match *a[0].clone() {
        List(ref lst) => {
            Ok(_int(lst.len().to_int().unwrap()))
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
                0 => Ok(_true()),
                _ => Ok(_false()),
            }
        },
        _ => Err("empty? called on non-sequence".to_string()),
    }
}



pub fn ns() -> HashMap<String,MalVal> {
    let mut ns: HashMap<String,MalVal> = HashMap::new();;

    ns.insert("=".to_string(), func(equal_q));

    ns.insert("pr-str".to_string(), func(pr_str));
    ns.insert("str".to_string(), func(str));
    ns.insert("prn".to_string(), func(prn));
    ns.insert("println".to_string(), func(println));
    ns.insert("read-string".to_string(), func(read_string));
    ns.insert("slurp".to_string(), func(slurp));

    ns.insert("<".to_string(),  func(lt));
    ns.insert("<=".to_string(), func(lte));
    ns.insert(">".to_string(),  func(gt));
    ns.insert(">=".to_string(), func(gte));
    ns.insert("+".to_string(), func(add));
    ns.insert("-".to_string(), func(sub));
    ns.insert("*".to_string(), func(mul));
    ns.insert("/".to_string(), func(div));
    ns.insert("time-ms".to_string(), func(time_ms));

    ns.insert("list".to_string(), func(_list));
    ns.insert("list?".to_string(), func(list_q));
    ns.insert("cons".to_string(), func(cons));
    ns.insert("concat".to_string(), func(concat));
    ns.insert("empty?".to_string(), func(empty_q));
    ns.insert("nth".to_string(), func(nth));
    ns.insert("first".to_string(), func(first));
    ns.insert("rest".to_string(), func(rest));
    ns.insert("count".to_string(), func(count));

    return ns;
}
