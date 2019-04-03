use super::types;
use super::types::MalType;
use std::collections::HashMap;
use std::error::Error;
use std::fmt;

pub fn core() -> HashMap<&'static str, MalType> {
    let mut core_fns = HashMap::new();
    core_fns.insert("+", MalType::Fn(std::sync::Arc::new(add)));
    core_fns.insert("-", MalType::Fn(std::sync::Arc::new(sub)));
    core_fns.insert("*", MalType::Fn(std::sync::Arc::new(mul)));
    core_fns.insert("/", MalType::Fn(std::sync::Arc::new(div)));
    core_fns.insert("<", MalType::Fn(std::sync::Arc::new(lt)));
    core_fns.insert("<=", MalType::Fn(std::sync::Arc::new(lte)));
    core_fns.insert("=", MalType::Fn(std::sync::Arc::new(eq)));
    core_fns.insert(">", MalType::Fn(std::sync::Arc::new(gt)));
    core_fns.insert(">=", MalType::Fn(std::sync::Arc::new(gte)));
    core_fns.insert("list?", MalType::Fn(std::sync::Arc::new(is_list)));
    core_fns.insert("empty?", MalType::Fn(std::sync::Arc::new(is_empty)));
    core_fns.insert("list", MalType::Fn(std::sync::Arc::new(list)));
    core_fns.insert("count", MalType::Fn(std::sync::Arc::new(count)));
    core_fns.insert("not", MalType::Fn(std::sync::Arc::new(not)));
    core_fns.insert("pr-str", MalType::Fn(std::sync::Arc::new(pr_str)));
    core_fns.insert("str", MalType::Fn(std::sync::Arc::new(str_fn)));
    core_fns.insert("prn", MalType::Fn(std::sync::Arc::new(prn)));
    core_fns.insert("println", MalType::Fn(std::sync::Arc::new(println)));

    core_fns
}

#[derive(Debug)]
struct CoreError {
    err: String,
}

impl CoreError {
    fn new(err: &str) -> types::Result {
        Err(Box::new(Self {
            err: err.to_owned(),
        }))
    }
}

impl Error for CoreError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        None
    }
}

impl fmt::Display for CoreError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Core error: {}", self.err)
    }
}

fn add(params: &[MalType]) -> types::Result {
    let mut sum = 0;
    for p in params {
        match p {
            MalType::Integer(i) => sum += i,
            _ => return CoreError::new("Can only sum numeric"),
        }
    }

    Ok(MalType::Integer(sum))
}

fn sub(params: &[MalType]) -> types::Result {
    if let MalType::Integer(init) = params[0] {
        let mut res = init;
        for p in params[1..].iter() {
            match p {
                MalType::Integer(i) => res -= i,
                _ => return CoreError::new("Can only sum numeric"),
            }
        }
        Ok(MalType::Integer(res))
    } else {
        CoreError::new("Can only sum numeric")
    }
}

fn mul(params: &[MalType]) -> types::Result {
    let mut res = 1;
    for p in params {
        match p {
            MalType::Integer(i) => res *= i,
            _ => return CoreError::new("Can only sum numeric"),
        }
    }

    Ok(MalType::Integer(res))
}

fn div(params: &[MalType]) -> types::Result {
    if let MalType::Integer(init) = params[0] {
        let mut res = init;
        for p in params[1..].iter() {
            match p {
                MalType::Integer(i) => res /= i,
                _ => return CoreError::new("Can only div numeric"),
            }
        }
        Ok(MalType::Integer(res))
    } else {
        CoreError::new("Can only div numeric")
    }
}

fn compare<F>(params: &[MalType], cmp_fn: F) -> types::Result
where
    F: Fn(&isize, &isize) -> bool,
{
    if let (MalType::Integer(a), MalType::Integer(b)) = (&params[0], &params[1]) {
        Ok(MalType::Boolean(cmp_fn(a, b)))
    } else {
        CoreError::new("Can only compare numeric")
    }
}

fn lt(params: &[MalType]) -> types::Result {
    compare(params, |a, b| a < b)
}

fn lte(params: &[MalType]) -> types::Result {
    compare(params, |a, b| a <= b)
}

fn eq(params: &[MalType]) -> types::Result {
    Ok(MalType::Boolean(params[0] == params[1]))
}

fn gt(params: &[MalType]) -> types::Result {
    compare(params, |a, b| a > b)
}

fn gte(params: &[MalType]) -> types::Result {
    compare(params, |a, b| a >= b)
}

fn is_list(params: &[MalType]) -> types::Result {
    Ok(MalType::Boolean(match &params[0] {
        MalType::List(_) => true,
        _ => false,
    }))
}

fn is_empty(params: &[MalType]) -> types::Result {
    Ok(MalType::Boolean(match &params[0] {
        MalType::List(a) => a.len() == 0,
        MalType::Vector(a) => a.len() == 0,
        _ => true,
    }))
}

fn list(params: &[MalType]) -> types::Result {
    Ok(MalType::List(params.to_vec()))
}

fn count(params: &[MalType]) -> types::Result {
    Ok(MalType::Integer(match &params[0] {
        MalType::List(a) => a.len() as isize,
        MalType::Vector(a) => a.len() as isize,
        _ => 0,
    }))
}

fn not(params: &[MalType]) -> types::Result {
    Ok(MalType::Boolean(match &params[0] {
        MalType::Boolean(a) => !a,
        _ => false,
    }))
}

fn pr_str(params: &[MalType]) -> types::Result {
    let res = params
        .iter()
        .map(|p| super::printer::pr_str(&p, true))
        .collect::<Vec<String>>()
        .join(" ");
    Ok(MalType::String(res))
}

fn str_fn(params: &[MalType]) -> types::Result {
    let res = params
        .iter()
        .map(|p| super::printer::pr_str(&p, false))
        .collect::<Vec<String>>()
        .join("");
    Ok(MalType::String(res))
}

fn prn(params: &[MalType]) -> types::Result {
    print!("{:?}", pr_str(params).unwrap());
    Ok(MalType::Nil)
}

fn println(params: &[MalType]) -> types::Result {
    let res = params
        .iter()
        .map(|p| super::printer::pr_str(&p, false))
        .collect::<Vec<String>>()
        .join(" ");
    println!("{}", res);
    Ok(MalType::Nil)
}
