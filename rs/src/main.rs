
use caret::parse;

pub fn main() {
    let result = parse::parse("examples/syntax.def");
    match result {
        Ok(_) => println!("OK!"),
        Err(e) => println!("Failure while parsing: {}", e)
    }
}
