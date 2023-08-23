use crate::ast::{BinOp, Expr, Literal, UnOp};
use crate::compile::new_stream;

use chumsky::Parser;

#[test]
fn test_expr_literal() {
    assert_eq!(
        Expr::parser().parse(new_stream("1", "")),
        Ok(Expr::Literal(Literal::Integer(1))),
    );

    assert_eq!(
        Expr::parser().parse(new_stream("null", "")),
        Ok(Expr::Literal(Literal::Null)),
    )
}

#[test]
fn test_expr_ident() {
    assert_eq!(
        Expr::parser().parse(new_stream("hello", "")),
        Ok(Expr::Ident("hello".to_string()))
    );
}

#[test]
fn test_expr_unary() {
    assert_eq!(
        Expr::parser().parse(new_stream("!true", "")),
        Ok(Expr::Unary(UnOp::Inv, Box::new(Expr::Literal(Literal::Boolean(true)))))
    );

    assert_eq!(
        Expr::parser().parse(new_stream("- 1", "")),
        Ok(Expr::Unary(UnOp::Neg, Box::new(Expr::Literal(Literal::Integer(1)))))
    );
}

#[test]
fn test_expr_binary() {
    assert_eq!(
        Expr::parser().parse(new_stream("null == null", "")),
        Ok(Expr::Binary(
            Box::new(Expr::Literal(Literal::Null)),
            BinOp::Is,
            Box::new(Expr::Literal(Literal::Null))),
        )
    );

    assert_eq!(
        Expr::parser().parse(new_stream("2 % 3", "")),
        Ok(Expr::Binary(
            Box::new(Expr::Literal(Literal::Integer(2))),
            BinOp::Rem,
            Box::new(Expr::Literal(Literal::Integer(3))),
        ))
    );
}
