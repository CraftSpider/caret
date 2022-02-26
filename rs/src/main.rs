
use caret::parse;

pub fn main() {
    let result = parse::parse("examples/syntax.def");
    println!("{:?}", result);
}
