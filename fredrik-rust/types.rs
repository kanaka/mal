use std::error::Error;
use std::fmt;
pub type Result = std::result::Result<MalType, Box<Error>>;

#[derive(Clone)]
pub enum MalType {
    List(Vec<MalType>),
    Vector(Vec<MalType>),
    HashMap(Vec<MalType>),
    Nil,
    Symbol(String),
    Boolean(bool),
    Integer(isize),
    String(String),
    Quote(Box<MalType>),
    QuasiQuote(Box<MalType>),
    UnQuote(Box<MalType>),
    SpliceUnQuote(Box<MalType>),
    WithMeta(Box<MalType>, Box<MalType>),
    Deref(String),
    Fn(std::sync::Arc<Fn(&[MalType]) -> Result>),
}

impl fmt::Debug for MalType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            MalType::List(lst) => write!(f, "List({:?})", lst),
            MalType::Vector(lst) => write!(f, "Vector({:?})", lst),
            MalType::HashMap(lst) => write!(f, "HashMap({:?})", lst),
            MalType::Nil => write!(f, "Nil"),
            MalType::Symbol(s) => write!(f, "Symbol({})", s),
            MalType::Boolean(b) => write!(f, "Boolean({})", b),
            MalType::Integer(i) => write!(f, "Integer({})", i),
            MalType::String(v) => write!(f, "String({})", v),
            MalType::Quote(v) => write!(f, "Quote({:?})", v),
            MalType::QuasiQuote(v) => write!(f, "QuasiQuote({:?})", v),
            MalType::UnQuote(v) => write!(f, "UnQuote({:?})", v),
            MalType::SpliceUnQuote(v) => write!(f, "SpliceUnQuote({:?})", v),
            MalType::WithMeta(a, b) => write!(f, "WithMeta({:?}, {:?})", a, b),
            MalType::Deref(v) => write!(f, "Deref({})", v),
            MalType::Fn(_) => write!(f, "Fn"),
        }
    }
}

impl PartialEq for MalType {
    fn eq(&self, other: &MalType) -> bool {
        match (self, other) {
            (MalType::List(a), MalType::List(b)) => a == b,
            (MalType::Vector(a), MalType::Vector(b)) => a == b,
            (MalType::List(a), MalType::Vector(b)) => a == b,
            (MalType::Vector(a), MalType::List(b)) => a == b,
            (MalType::Integer(a), MalType::Integer(b)) => a == b,
            (MalType::String(a), MalType::String(b)) => a == b,
            (MalType::Boolean(a), MalType::Boolean(b)) => a == b,
            (MalType::Nil, MalType::Nil) => true,
            _ => false,
        }
    }
}
