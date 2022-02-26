
use crate::ast::{CtBinOp, CtExpr, CtUnaryOp};
use super::literal::literal;

use chumsky::prelude::*;

fn unary_op() -> impl Parser<char, CtUnaryOp, Error = Simple<char>> + Clone {
    just("-").to(CtUnaryOp::Neg)
        .or(just("!").to(CtUnaryOp::Neg))
}

fn product_op() -> impl Parser<char, CtBinOp, Error = Simple<char>> + Clone {
    just("*").to(CtBinOp::Mul)
        .or(just("/").to(CtBinOp::Div))
        .or(just("%").to(CtBinOp::Rem))
}

fn sum_op() -> impl Parser<char, CtBinOp, Error = Simple<char>> + Clone {
    just("+").to(CtBinOp::Add)
        .or(just("-").to(CtBinOp::Sub))
}

fn cmp_op() -> impl Parser<char, CtBinOp, Error = Simple<char>> + Clone {
    just("==").to(CtBinOp::Is)
        .or(just("<=").to(CtBinOp::Le))
        .or(just(">=").to(CtBinOp::Ge))
        .or(just("<").to(CtBinOp::Lt))
        .or(just(">").to(CtBinOp::Gt))
}

fn and_op() -> impl Parser<char, CtBinOp, Error = Simple<char>> + Clone {
    just("and").to(CtBinOp::And)
}

fn or_op() -> impl Parser<char, CtBinOp, Error = Simple<char>> + Clone {
    just("or").to(CtBinOp::Or)
}

fn array(expr: impl Parser<char, CtExpr, Error = Simple<char>> + Clone) -> impl Parser<char, Vec<CtExpr>, Error = Simple<char>> + Clone {
    expr.separated_by(just(',')).padded().delimited_by('[', ']')
}

fn atom(expr: impl Parser<char, CtExpr, Error = Simple<char>> + Clone) -> impl Parser<char, CtExpr, Error = Simple<char>> + Clone {
    text::ident().map(CtExpr::Ident)
        .or(array(expr.clone()).map(CtExpr::Array))
        .or(literal().map(CtExpr::Literal))
        .or(expr.delimited_by('(', ')').map(|e| CtExpr::Parens(Box::new(e))))
}

pub fn expr() -> impl Parser<char, CtExpr, Error = Simple<char>> {
    recursive(|expr| {
        let unary = unary_op()
            .repeated()
            .then(atom(expr))
            .foldr(|op, rhs| CtExpr::Unary(op, Box::new(rhs)));

        let precedence = [
            product_op().boxed(),
            sum_op().boxed(),
            cmp_op().boxed(),
            and_op().boxed(),
            or_op().boxed(),
        ];

        let mut last = unary.boxed();

        for ops in precedence {
            last = last.clone()
                .then(ops.padded().then(last).repeated())
                .foldl(|lhs, (op, rhs)| CtExpr::Binary(Box::new(lhs), op, Box::new(rhs)))
                .boxed();
        }

        last
    })
}
