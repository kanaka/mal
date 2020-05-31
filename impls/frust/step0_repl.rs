const HISTORY: &str = "history.txt";

fn main() {
    let rl_config = rustyline::Config::builder()
        .completion_type(rustyline::CompletionType::Circular)
        .edit_mode(rustyline::EditMode::Vi)
        .color_mode(rustyline::ColorMode::Enabled)
        .build();
    let mut rl = rustyline::Editor::<()>::with_config(rl_config);
    let _ = rl.load_history(HISTORY);
    for readline in rl.iter("user> ") {
        match readline {
            Ok(s) => println!("{}", rep(s)),
            Err(e) => eprintln!("{}", e),
        }
    }
    rl.save_history(HISTORY).expect("Failed to save history");
}

fn rep(s: String) -> String {
    let r = read(s);
    let e = eval(r);
    print(e)
}

fn read(s: String) -> String {
    s
}

fn eval(s: String) -> String {
    s
}

fn print(s: String) -> String {
    s
}
