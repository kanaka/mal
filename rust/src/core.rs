#![allow(dead_code)]

extern crate time;
use std::collections::HashMap;
use std::io::File;

use types::{MalVal,MalRet,err_val,err_str,err_string,
            Int,Strn,List,Vector,Hash_Map,
            _nil,_true,_false,_int,string,list,func};
use types;
use readline;
use reader;
use printer;

// General functions
fn equal_q(a:Vec<MalVal>) -> MalRet {
    if a.len() != 2 {
        return err_str("Wrong arity to equal? call");
    }
    match a[0] == a[1] {
        true => Ok(_true()),
        false => Ok(_false()),
    }
}

// Errors/Exceptions
fn throw(a:Vec<MalVal>) -> MalRet {
    if a.len() != 1 {
        return err_str("Wrong arity to throw call");
    }
    err_val(a[0].clone())
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

fn readline(a:Vec<MalVal>) -> MalRet {
    match *a[0] {
        Strn(ref a0) => match readline::mal_readline(a0.as_slice()) {
            Some(line) => Ok(string(line)),
            None       => err_val(_nil()),
        },
        _ => err_str("read_string called with non-string"),
    }
}

fn read_string(a:Vec<MalVal>) -> MalRet {
    match *a[0] {
        Strn(ref a0) => reader::read_str(a0.to_string()),
        _ => err_str("read_string called with non-string"),
    }
}

fn slurp(a:Vec<MalVal>) -> MalRet {
    match *a[0] {
        Strn(ref a0) => {
            match File::open(&Path::new(a0.as_slice())).read_to_string() {
                Ok(s) => Ok(string(s)),
                Err(e) => err_string(e.to_string()),
            }
        },
        _ => err_str("slurp called with non-string"),
    }
}


// Numeric functions
fn int_op(f: |i:int,j:int|-> int, a:Vec<MalVal>) -> MalRet {
    match *a[0] {
        Int(a0) => match *a[1] {
            Int(a1) => Ok(_int(f(a0,a1))),
            _ => err_str("second arg must be an int"),
        },
        _ => err_str("first arg must be an int"),
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
            _ => err_str("second arg must be an int"),
        },
        _ => err_str("first arg must be an int"),
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


// Hash Map functions
pub fn get(a:Vec<MalVal>) -> MalRet {
    if a.len() != 2 {
        return err_str("Wrong arity to get call");
    }
    let a0 = a[0].clone();
    let hm: &HashMap<String,MalVal> = match *a0 {
        Hash_Map(ref hm) => hm,
        _ => return err_str("get of non-hash map"),
    };
    match *a[1] {
        Strn(ref key) => {
            match hm.find_copy(key) {
                Some(v) => Ok(v),
                None    => Ok(_nil()),
            }
        },
        _ => return err_str("get with non-string key"),
    }
}


// Sequence functions
pub fn cons(a:Vec<MalVal>) -> MalRet {
    match *a[1] {
        List(ref v) | Vector(ref v) => {
            let mut new_v = v.clone();
            new_v.insert(0, a[0].clone());
            Ok(list(new_v))
        },
        _ => err_str("Second arg to cons not a sequence"),
    }
}

pub fn concat(a:Vec<MalVal>) -> MalRet {
    let mut new_v:Vec<MalVal> = vec![];
    for lst in a.iter() {
        match **lst {
            List(ref l) | Vector(ref l) => {
                new_v.push_all(l.as_slice());
            },
            _ => return err_str("concat called with non-sequence"),
        }
    }
    Ok(list(new_v))
}

pub fn nth(a:Vec<MalVal>) -> MalRet {
    if a.len() != 2 {
        return err_str("Wrong arity to nth call");
    }
    let a0 = a[0].clone();
    let a1 = a[1].clone();
    let seq = match *a0 {
        List(ref v) | Vector(ref v) => v,
        _ => return err_str("nth called with non-sequence"),
    };
    let idx = match *a1 {
        Int(i) => {
            match i.to_uint() {
                Some(ui) => ui,
                None => return Ok(_nil()),
            }
        },
        _ => return err_str("nth called with non-integer index"),
    };
    if idx >= seq.len() {
        Ok(_nil())
    } else {
        Ok(seq[idx].clone())
    }
}

pub fn first(a:Vec<MalVal>) -> MalRet {
    if a.len() != 1 {
        return err_str("Wrong arity to first call");
    }
    let a0 = a[0].clone();
    let seq = match *a0 {
        List(ref v) | Vector(ref v) => v,
        _ => return err_str("first called with non-sequence"),
    };
    if seq.len() == 0 {
        Ok(_nil())
    } else {
        Ok(seq[0].clone())
    }
}

pub fn rest(a:Vec<MalVal>) -> MalRet {
    if a.len() != 1 {
        return err_str("Wrong arity to rest call");
    }
    let a0 = a[0].clone();
    let seq = match *a0 {
        List(ref v) | Vector(ref v) => v,
        _ => return err_str("rest called with non-sequence"),
    };
    if seq.len() == 0 {
        Ok(list(vec![]))
    } else {
        Ok(list(seq.slice(1,seq.len()).to_vec()))
    }
}

pub fn empty_q(a:Vec<MalVal>) -> MalRet {
    if a.len() != 1 {
        return err_str("Wrong arity to empty? call");
    }
    match *a[0].clone() {
        List(ref v) | Vector(ref v) => {
            match v.len() {
                0 => Ok(_true()),
                _ => Ok(_false()),
            }
        },
        _ => err_str("empty? called on non-sequence"),
    }
}

pub fn count(a:Vec<MalVal>) -> MalRet {
    if a.len() != 1 {
        return err_str("Wrong arity to count call");
    }
    match *a[0].clone() {
        List(ref v) | Vector(ref v) => {
            Ok(_int(v.len().to_int().unwrap()))
        },
        _ => err_str("count called on non-sequence"),
    }
}

pub fn apply(a:Vec<MalVal>) -> MalRet {
    if a.len() < 2 {
        return err_str("apply call needs 2 or more arguments");
    }
    let ref f = a[0];
    let mut args = a.slice(1,a.len()-1).to_vec();
    match *a[a.len()-1] {
        List(ref v) | Vector(ref v) => {
            args.push_all(v.as_slice());
            f.apply(args)
        },
        _ => err_str("apply call with non-sequence"),
    }
}

pub fn map(a:Vec<MalVal>) -> MalRet {
    if a.len() != 2 {
        return err_str("Wrong arity to map call");
    }
    let mut results:Vec<MalVal> = vec![];
    let ref f = a[0].clone();
    let seq = a[1].clone();
    match *seq {
        List(ref v) | Vector(ref v) => {
            for mv in v.iter() {
                match f.apply(vec![mv.clone()]) {
                    Ok(res) => results.push(res),
                    Err(e) => return Err(e),
                }
            }
        },
        _ => return err_str("map call with non-sequence"),
    }
    Ok(list(results))
}



pub fn ns() -> HashMap<String,MalVal> {
    let mut ns: HashMap<String,MalVal> = HashMap::new();;

    ns.insert("=".to_string(), func(equal_q));
    ns.insert("throw".to_string(), func(throw));
    ns.insert("nil?".to_string(), func(types::nil_q));
    ns.insert("true?".to_string(), func(types::true_q));
    ns.insert("false?".to_string(), func(types::false_q));
    ns.insert("symbol?".to_string(), func(types::symbol_q));

    ns.insert("pr-str".to_string(), func(pr_str));
    ns.insert("str".to_string(), func(str));
    ns.insert("prn".to_string(), func(prn));
    ns.insert("println".to_string(), func(println));
    ns.insert("readline".to_string(), func(readline));
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

    ns.insert("list".to_string(), func(types::listv));
    ns.insert("list?".to_string(), func(types::list_q));
    ns.insert("vector".to_string(), func(types::vectorv));
    ns.insert("vector?".to_string(), func(types::vector_q));
    ns.insert("hash-map".to_string(), func(types::hash_mapv));
    ns.insert("map?".to_string(), func(types::hash_map_q));
    ns.insert("get".to_string(), func(get));

    ns.insert("sequential?".to_string(), func(types::sequential_q));
    ns.insert("cons".to_string(), func(cons));
    ns.insert("concat".to_string(), func(concat));
    ns.insert("empty?".to_string(), func(empty_q));
    ns.insert("nth".to_string(), func(nth));
    ns.insert("first".to_string(), func(first));
    ns.insert("rest".to_string(), func(rest));
    ns.insert("count".to_string(), func(count));
    ns.insert("apply".to_string(), func(apply));
    ns.insert("map".to_string(), func(map));

    return ns;
}
