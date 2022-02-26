
use crate::ast::{CtExpr, CtType};
use super::expr::expr;

use chumsky::prelude::*;

fn builtin() -> impl Parser<char, CtType, Error = Simple<char>> {
    just("sint8").to(CtType::I8)
        .or(just("sint16").to(CtType::I16))
        .or(just("sint32").to(CtType::I32))
        .or(just("sint64").to(CtType::I64))
        .or(just("uint8").to(CtType::U8))
        .or(just("uint16").to(CtType::U16))
        .or(just("uint32").to(CtType::U32))
        .or(just("uint64").to(CtType::U64))
        .or(just("float32").to(CtType::F32))
        .or(just("float64").to(CtType::F64))
        .or(just("string").to(CtType::String))
        .or(just("bool").to(CtType::Bool))
        .or(just("null").to(CtType::Null))
}

#[derive(Debug, Clone)]
enum PtrArr {
    Arr(Option<CtExpr>),
    Ptr,
}

pub fn ty() -> impl Parser<char, CtType, Error = Simple<char>> {
    let following = expr().or_not().delimited_by('[', ']')
        .map(|e| PtrArr::Arr(e))
        .or(just('*').to(PtrArr::Ptr));

    builtin()
        .or(text::ident().map(|i| CtType::Section(i)))
        .then(following.repeated())
        .foldl(|ty, follow| match follow {
            PtrArr::Arr(expr) => CtType::Array(Box::new(ty), expr),
            PtrArr::Ptr => CtType::Pointer(Box::new(ty)),
        })
}
