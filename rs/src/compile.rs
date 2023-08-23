use crate::ast::File;
use crate::span::Span;
use crate::tokens::Token;

use chumsky::{Parser, Span as _, Stream};
use std::path::Path;

pub mod tokenize;
pub mod parse;

use tokenize::tokenize;

fn new_stream<'a>(
    src: &'a str,
    file_name: &'a str,
) -> Stream<'a, Token<'a>, Span<'a>, Box<dyn Iterator<Item = (Token<'a>, Span<'a>)> + 'a>> {
    let tokens = tokenize(&src);

    Stream::from_iter(
        Span::new(file_name, src.len()..src.len()),
        Box::new(
            tokens.map(move |(t, r)| (t, Span::new(file_name, r))),
        ),
    )
}

pub fn compile<P: AsRef<Path>>(p: P) -> File {
    // TODO: Error handling

    let file_name = p.as_ref().to_str().unwrap_or("<invalid file name>");

    let src = std::fs::read_to_string(p.as_ref()).unwrap();

    let x = match parse::parser().parse(new_stream(&src, file_name)) {
        Ok(a) => a,
        Err(errs) => {
            for e in errs {
                let span = e.span();
                println!(
                    "{} - {:?}\n-----\n{:?}\n-----",
                    e,
                    span,
                    &src[span.start().saturating_sub(10)..span.end().saturating_add(10)]
                );
            }
            panic!()
        }
    };

    x
}
