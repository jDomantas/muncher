use std::rc::Rc;
use muncher::Intrinsics;

struct Intr;

impl Intrinsics for Intr {
    fn print(&self, value: &str) {
        print!("{}", value);
    }
}

fn main() {
    let args = std::env::args().collect::<Vec<_>>();
    let file = match args.as_slice() {
        [_, file] => file,
        [exe, ..] => {
            eprintln!("usage: {} <file>", exe);
            std::process::exit(1);
        }
        [] => {
            eprintln!("usage: muncher <file>");
            std::process::exit(1);
        }
    };
    let source = match std::fs::read_to_string(file) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("failed to read {}", file);
            eprintln!("{}", e);
            std::process::exit(1);
        }
    };
    match muncher::eval(&source, Rc::new(Intr)) {
        Ok(()) => {}
        Err(e) => {
            eprintln!(
                "runtime error (at {}:{}): {}",
                e.span.start.line,
                e.span.start.col,
                e.msg,
            );
        }
    }
}
