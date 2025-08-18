extern crate rustyline;

// A global variable makes more sense than passing the readline editor
// as an argument to *every* core function just for readline.

struct S {
    e: rustyline::Editor<(), rustyline::history::DefaultHistory>,
}

impl Drop for S {
    fn drop(&mut self) {
        self.e.save_history(".mal-history").unwrap()
    }
}

thread_local! {
    static ED : std::cell::RefCell<S> = {
        let mut e = rustyline::Editor::new().unwrap();
        if e.load_history(".mal-history").is_err() {
            println!("No previous history.");
        }
        std::cell::RefCell::new(S{e})
    }
}

pub fn readline(prompt: &str) -> Option<String> {
    ED.with_borrow_mut(|s| {
        let r = s.e.readline(prompt);
        if let Err(rustyline::error::ReadlineError::Eof) = r {
            None
        } else {
            let mut line = r.unwrap();
            // Remove any trailing \n or \r\n
            while line.ends_with('\n') || line.ends_with('\r') {
                line.pop();
            }
            if !line.is_empty() {
                let _ = s.e.add_history_entry(&line);
            }
            Some(line.to_string())
        }
    })
}
