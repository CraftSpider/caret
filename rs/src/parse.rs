
use std::{fmt, fs, io};
use std::convert::TryFrom;

use pest::Parser;
use pest_derive::Parser;
use pest::iterators;

use crate::ast::*;

pub type Pairs<'a> = iterators::Pairs<'a, Rule>;
pub type Pair<'a> = iterators::Pair<'a, Rule>;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub enum Error {
    InvalidRule(Rule, Rule),
    MultipleEncodings,
    Io(io::Error),
    Pest(pest::error::Error<Rule>),
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Error {
        Error::Io(err)
    }
}

impl From<pest::error::Error<Rule>> for Error {
    fn from(err: pest::error::Error<Rule>) -> Error {
        Error::Pest(err)
    }
}

impl fmt::Display for Error {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::InvalidRule(expected, got) => write!(fmt, "Incorrect rule for AST node: Expected {:?}, got {:?}", expected, got),
            Error::MultipleEncodings => write!(fmt, "Only one encoding may be specified per file"),
            Error::Io(err) => write!(fmt, "IO Error: {}", err),
            Error::Pest(err) => write!(fmt, "Parsing Error: {}", err),
        }
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Error::Io(err) => Some(err),
            Error::Pest(err) => Some(err),
            _ => None,
        }
    }
}

#[derive(Parser)]
#[grammar = "caret.pest"]
struct CaretParser;

pub fn parse(filename: &str) -> Result<CtAst> {
    let contents = fs::read_to_string(filename)?;

    let mut input: Vec<_> = CaretParser::parse(Rule::tl, &contents)?.collect();
    assert_eq!(input.len(), 1);

    CtAst::try_from(input.remove(0))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_alias() {
        let result = CaretParser::parse(Rule::type_decl, "type Alias = sint32|float32");

        match result {
            Ok(r) => println!("{:?}", r),
            Err(e) => println!("{}", e)
        }
    }
}