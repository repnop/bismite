mod eval;
mod repl;

use repl::Repl;

fn main() {
    let mut repl = Repl::new();

    loop {
        if let Some(text) = repl.run() {
            println!("{}", text);
        }
    }
}
