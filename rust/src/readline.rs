// Based on: https://github.com/shaleh/rust-readline (MIT)
use libc;

use std::ffi::{CStr, CString};
use std::fs::{OpenOptions, File};
use std::io::BufReader;
use std::io::prelude::*;
use std::str;

mod ext_readline {
    extern crate libc;
    use self::libc::c_char;
    #[link(name = "readline")]
    extern {
        pub fn add_history(line: *const c_char);
        pub fn readline(p: *const c_char) -> *const c_char;
    }
}

pub fn add_history(line: &str) {
    unsafe {
        ext_readline::add_history(CString::new(line).unwrap().as_ptr());
    }
}

pub fn readline(prompt: &str) -> Option<String> {
    let cprmt = CString::new(prompt).unwrap();
    unsafe {
        let ptr = ext_readline::readline(cprmt.as_ptr());
        if ptr.is_null() {  // user pressed Ctrl-D
            None
        } else {
            let ret = str::from_utf8(CStr::from_ptr(ptr).to_bytes());
            let ret = ret.ok().map(|s| s.to_string());
            libc::free(ptr as *mut _);
            return ret;
        }
    }
}

// --------------------------------------------

static mut history_loaded : bool = false;
static HISTORY_FILE: &'static str = "/home/joelm/.mal-history";

fn load_history() {
    unsafe {
        if history_loaded { return; }
        history_loaded = true;
    }

    let file = match File::open(HISTORY_FILE) {
        Ok(f) => f,
        Err(..) => return
    };
    let file = BufReader::new(file);
    for line in file.lines() {
        let rt: &[_] = &['\r', '\n'];
        let line2 = line.unwrap();
        let line3 = line2.trim_right_matches(rt);
        add_history(line3);
    }
}

fn append_to_history(line: &str) {
    let file = OpenOptions::new().append(true).write(true).create(true)
                                 .open(HISTORY_FILE);
    let mut file = match file { Ok(f) => f, Err(..) => return };
    let _ = file.write_all(line.as_bytes());
    let _ = file.write_all(b"\n");
}

pub fn mal_readline (prompt: &str) -> Option<String> {
    load_history();
    let line = readline(prompt);
    if let Some(ref s) = line {
        add_history(s);
        append_to_history(s);
    }
    line
}
