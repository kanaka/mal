use std::error::Error;
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
    BIF(fn(&[MalType]) -> Result),
}
