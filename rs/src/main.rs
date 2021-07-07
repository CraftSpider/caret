
use caret::parse;

pub fn main() {
    let result = parse::parse("examples/syntax.def");
    match result {
        Ok(ast) => println!("{:#?}", ast),
        Err(e) => println!("Failure while parsing: {}", e)
    }
}
