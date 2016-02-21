extern crate libc;
extern crate regex;
extern crate time;
extern crate num;

macro_rules! regex {
    ($e:expr) => (::regex::Regex::new($e).unwrap())
}

pub mod core;
pub mod env;
pub mod printer;
pub mod reader;
pub mod readline;
pub mod types;
