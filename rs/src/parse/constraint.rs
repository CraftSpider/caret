
use crate::ast::{CtConstraint, CtTyConstraint, CtCondition, CtType, CtExpr};
use super::ty::ty;
use super::literal::literal;
use super::expr::expr;

use chumsky::prelude::*;

fn conditional(ty_constraint: impl Parser<char, CtTyConstraint, Error = Simple<char>> + Clone) -> impl Parser<char, CtCondition, Error = Simple<char>> {
    just('?')
        .padded()
        .ignore_then(expr().padded())
        .then_ignore(just('<').padded())
        .then(ty_constraint.clone().padded())
        .then_ignore(just('>').padded())
        .then_ignore(just('<').padded())
        .then(ty_constraint.padded())
        .then_ignore(just('>').padded())
        .map(|((condition, left), right)| CtCondition {
            condition,
            left: Box::new(left),
            right: Box::new(right),
        })
}

fn global() -> impl Parser<char, CtType, Error = Simple<char>> {
    just('@')
        .padded()
        .ignore_then(ty())
}

pub fn ty_constraint() -> impl Parser<char, CtTyConstraint, Error = Simple<char>> {
    recursive(|ty_constraint| {
        literal()
            .map(|l| CtTyConstraint::Literal(l))
            .or(ty().map(|t| CtTyConstraint::Type(t)))
            .or(conditional(ty_constraint).map(|c| CtTyConstraint::Conditional(c)))
            .or(global().map(|g| CtTyConstraint::Global(g)))
            .separated_by(just('|').padded())
            .map(|mut u| if u.len() == 1 {
                u.remove(0)
            } else {
                CtTyConstraint::Union(u)
            })
    })
}

fn val_constraint() -> impl Parser<char, CtExpr, Error = Simple<char>> {
    expr()
}

pub fn constraint() -> impl Parser<char, CtConstraint, Error = Simple<char>> {
    text::ident()
        .padded()
        .or_not()
        .then_ignore(just(':').padded())
        .then(ty_constraint().padded())
        .then(
            just("->")
                .padded()
                .ignore_then(val_constraint())
                .or_not()
        )
        .map(|((name, ty_constraint), val_constraint)| CtConstraint {
            name,
            ty_constraint,
            val_constraint,
        })
}
