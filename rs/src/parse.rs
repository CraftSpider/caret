
use crate::ast::*;

use std::path::Path;

use chumsky::prelude::*;

mod literal;
mod ty;
mod expr;
mod constraint;
mod top_level;

fn parser() -> impl Parser<char, CtAst, Error = Simple<char>> {
    top_level::top_level()
        .repeated()
        .map(|tl| CtAst { inner: tl })
        .then_ignore(end())
}

pub fn parse<P: AsRef<Path>>(p: P) -> CtAst {
    // TODO: Error handling
    let src = std::fs::read_to_string(p)
        .unwrap();
    match parser().parse(&*src) {
        Ok(a) => a,
        Err(errs) => {
            for e in errs {
                let span = e.span();
                println!("{} - {:?}\n-----\n{}\n-----", e, span, &src[span.start - 10 .. span.end + 10]);
            }
            panic!()
        }
    }
}
