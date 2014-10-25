// Based on: https://github.com/shaleh/rust-readline (MIT)
extern crate libc;

use std::c_str;

use std::io::{File, Append, Write};
use std::io::BufferedReader;

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
        ext_readline::add_history(line.to_c_str().as_ptr());
    }
}

pub fn readline(prompt: &str) -> Option<String> {
    let cprmt = prompt.to_c_str();
    unsafe {
        let ret = ext_readline::readline(cprmt.as_ptr());
        if ret.is_null() {  // user pressed Ctrl-D
            None
        }
        else {
            c_str::CString::new(ret, true).as_str().map(|ret| ret.to_string())
        }
    }
}

// --------------------------------------------

static mut history_loaded : bool = false;
static HISTORY_FILE : &'static str = "/home/joelm/.mal-history";

fn load_history() {
    unsafe {
        if history_loaded { return; }
        history_loaded = true;
    }

    let path = Path::new(HISTORY_FILE);
    let mut file = BufferedReader::new(File::open(&path));
    for line in file.lines() {
        let rt: &[_] = &['\r', '\n'];
        let line2 = line.unwrap();
        let line3 = line2.as_slice().trim_right_chars(rt);
        add_history(line3);
    }
}

fn append_to_history(line: &str) {
    let path = Path::new("/home/joelm/.mal-history");
    let mut file = File::open_mode(&path, Append, Write);
    let _ = file.write_line(line);
}

pub fn mal_readline (prompt: &str) -> Option<String> {
    load_history();
    let line = readline(prompt);
    match line {
        None => None,
        _ => {
            add_history(line.clone().unwrap().as_slice());
            append_to_history(line.clone().unwrap().as_slice());
            line
        }
    }
}
