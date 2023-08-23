use crate::ast::*;
use crate::tokens::{Token, CharToken, RawInt, RawFloat, RawChar, RawString};
use crate::span::Span;

use chumsky::prelude::{Parser as _, Simple, end, filter, filter_map, just, recursive};
use chumsky::error::Error as _;
use std::convert::TryFrom;
use std::str::FromStr;

#[cfg(test)]
mod tests;

pub(crate) type Error<'a> = Simple<Token<'a>, Span<'a>>;

// Make this an alias once that's stable
pub trait Parser<'a, O>: chumsky::Parser<Token<'a>, O, Error = Error<'a>> {}

impl<'a, P, O> Parser<'a, O> for P
where
    P: chumsky::Parser<Token<'a>, O, Error = Error<'a>>
{}

pub fn newlines<'a>() -> impl Parser<'a, ()> {
    filter(Token::is_newline).repeated().to(())
}

pub fn parser<'a>() -> impl Parser<'a, File> {
    File::parser()
}

impl File {
    fn parser<'a>() -> impl Parser<'a, File> {
        TopLevel::parser()
            .repeated()
            .map(|tl| File { inner: tl })
            .then_ignore(end())
    }
}

impl TopLevel {
    fn parser<'a>() -> impl Parser<'a, TopLevel> {
        newlines()
            .ignore_then(
                Encoding::parser()
                    .map(|e| TopLevel::Encoding(e))
                    .or(TypeDecl::parser().map(|t| TopLevel::Type(t)))
                    .or(Section::parser().map(|s| TopLevel::Section(s)))
            )
            .then_ignore(newlines())
    }
}

impl Encoding {
    fn parser<'a>() -> impl Parser<'a, Encoding> {
        filter(Token::is_ident_lit("encoding"))
            .ignore_then(filter_map(Token::map_ident))
            .map(|encoding| Encoding { encoding })
    }
}

impl TypeDecl {
    fn parser<'a>() -> impl Parser<'a, TypeDecl> {
        filter(Token::is_ident_lit("type"))
            .ignore_then(
                filter_map(Token::map_ident)
                    .then_ignore(just(Token::Eq))
                    .then(TyConstraint::parser())
                    .then(filter_map(Token::map_ident).or_not())
                    .map(|((ty, constraint), extra)| {
                        TypeDecl::Alias(ty, constraint, extra.unwrap_or_default())
                    })
                    .or(Type::parser()
                        .then(filter_map(Token::map_ident))
                        .map(|(ty, m)| TypeDecl::Rule(ty, m))
                    )
            )
    }
}

impl Section {
    fn parser<'a>() -> impl Parser<'a, Section> {
        filter(Token::is_ident_lit("section"))
            .to(SectionKind::Section)
            .or(filter(Token::is_ident_lit("file")).to(SectionKind::File))
            .then(filter_map(Token::map_ident))
            .then(
                newlines()
                .ignore_then(Constraint::parser().then_ignore(newlines()).repeated())
                .then_ignore(newlines())
                .delimited_by(just(Token::OpenBrace), just(Token::CloseBrace))
            )
            .map(|((kind, name), constraints)| Section {
                kind,
                name,
                constraints,
            })
    }
}

impl Constraint {
    fn parser<'a>() -> impl Parser<'a, Constraint> {
        filter(Token::is_ident)
            .or_not()
            .then_ignore(just(Token::Colon))
            .then(TyConstraint::parser())
            .then(
                just(Token::RightArrow)
                    .ignore_then(Expr::parser())
                    .or_not(),
            )
            .map(|((name, ty_constraint), val_constraint)| {
                let name = match name {
                    Some(Token::Ident(i)) => Some(i.to_string()),
                    _ => None,
                };
                Constraint {
                    name,
                    ty_constraint,
                    val_constraint,
                }
            })
    }
}

impl TyConstraint {
    fn parser<'a>() -> impl Parser<'a, TyConstraint> {
        recursive(|ty_constraint| {
            Type::parser().map(|t| TyConstraint::Type(t))
                .or(Literal::parser().map(|l| TyConstraint::Literal(l)))
                .or(Condition::parser(ty_constraint).map(|c| TyConstraint::Conditional(c)))
                .or(just(Token::At).ignore_then(Type::parser()).map(|g| TyConstraint::Global(g)))
                .separated_by(just(Token::Pipe))
                .map(|mut u| {
                    if u.len() == 1 {
                        u.remove(0)
                    } else {
                        TyConstraint::Union(u)
                    }
                })
        })
    }
}

impl Literal {
    fn unescape<'a>(span: Span<'a>, str: &[CharToken<'a>]) -> Result<String, Error<'a>> {
        str.iter()
            .map(|c| c.to_char().map_err(|e| Error::custom(c.span_of(span.clone()), format!("Invalid unicode escape {:04x}", e))) )
            .collect()
    }

    fn unescape_char<'a>(span: Span<'a>, c: &CharToken<'a>) -> Result<char, Error<'a>> {
        c.to_char().map_err(|e| Error::custom(c.span_of(span), format!("Invalid unicode escape {:04x}", e)))
    }

    fn int_parser<'a>() -> impl Parser<'a, i128> + Clone {
        filter_map(|span, t| match t {
            Token::Int(RawInt {
                           neg,
                           base,
                           val,
                           suffix: _,
                       }) => {
                let base = match base {
                    Some("0b") => 2,
                    Some("0o") => 8,
                    Some("0x") => 16,
                    Some(base) => {
                        return Err(Error::custom(
                            span,
                            format!("Invalid base for integer literal: \"{}\"", base),
                        ))
                    }
                    None => 10,
                };

                let out = i128::from_str_radix(val, base).unwrap();

                Ok(if neg { -out } else { out })
            }
            _ => Err(Error::expected_input_found(
                span,
                [Some(Token::expect_int())],
                Some(t),
            )),
        })
    }

    fn float_parser<'a>() -> impl Parser<'a, f64> + Clone {
        filter_map(|span, t| match t {
            Token::Float(RawFloat {
                             neg,
                             val,
                             suffix: _,
                         }) => {
                let out = f64::from_str(val)
                    .map_err(|_| Error::custom(span, format!("Invalid float value {}", val)))?;
                Ok(if neg { -out } else { out })
            }
            _ => Err(Error::expected_input_found(
                span,
                [Some(Token::expect_float())],
                Some(t),
            )),
        })
    }

    fn byte_str_parser<'a>() -> impl Parser<'a, Vec<u8>> + Clone {
        filter_map(|span: Span<'_>, t| match t {
            Token::String(RawString { prefix: None::<>, val }) => Literal::unescape(span.clone(), &val)?
                .chars()
                .map(|c| {
                    u8::try_from(c as u32).map_err(|_| {
                        Error::custom(span.clone(), format!("Invalid byte value {:x}", c as u32))
                    })
                })
                .collect::<Result<_, _>>(),
            _ => Err(Error::expected_input_found(
                span,
                [Some(Token::expect_str())],
                Some(t),
            )),
        })
    }

    fn unicode_str_parser<'a>() -> impl Parser<'a, String> + Clone {
        filter_map(|span, t| match t {
            Token::String(RawString {
                              prefix: Some("u"),
                              val,
                          }) => Ok(Literal::unescape(span, &val)?),
            _ => Err(Error::expected_input_found(
                span,
                [Some(Token::expect_unicode_str())],
                Some(t),
            )),
        })
    }

    fn regex_str_parser<'a>() -> impl Parser<'a, String> + Clone {
        filter_map(|span, t| match t {
            Token::String(RawString {
                              prefix: Some("r"),
                              val,
                          }) => Ok(Literal::unescape(span, &val)?),
            _ => Err(Error::expected_input_found(
                span,
                [Some(Token::expect_regex_str())],
                Some(t),
            )),
        })
    }

    fn byte_char_parser<'a>() -> impl Parser<'a, u8> + Clone {
        filter_map(|span: Span<'_>, t| match t {
            Token::Char(RawChar { prefix: None::<>, val }) => {
                let c = Literal::unescape_char(span.clone(), &val)?;
                u8::try_from(c as u32)
                    .map_err(|_| Error::custom(span, format!("Invalid byte value {:x}", c as u32)))
            }
            _ => Err(Error::expected_input_found(
                span,
                [Some(Token::expect_char())],
                Some(t),
            )),
        })
    }

    fn unicode_char_parser<'a>() -> impl Parser<'a, char> + Clone {
        filter_map(|span, t| match t {
            Token::Char(RawChar {
                            prefix: Some("u"),
                            val,
                        }) => Ok(Literal::unescape_char(span, &val)?),
            _ => Err(Error::expected_input_found(
                span,
                [Some(Token::expect_unicode_char())],
                Some(t),
            )),
        })
    }

    fn bool_parser<'a>() -> impl Parser<'a, bool> + Clone {
        just(Token::True).to(true).or(just(Token::False).to(false))
    }

    /*fn array_lit(literal: impl Parser<'a, CtLiteral>) -> impl Parser<Input<'a>, Vec<CtLiteral>, Error = Error<'a>> {
        just('[')
            .ignore_then(literal.separated_by(just(',').padded()))
            .then_ignore(just(']'))
    }*/

    fn parser<'a>() -> impl Parser<'a, Literal> + Clone {
        recursive(|_| {
            Literal::float_parser()
                .map(Literal::Float)
                .or(Literal::int_parser().map(Literal::Integer))
                .or(Literal::byte_str_parser().map(Literal::ByteString))
                .or(Literal::unicode_str_parser().map(Literal::UnicodeString))
                .or(Literal::regex_str_parser().map(Literal::Regex))
                .or(Literal::byte_char_parser().map(Literal::ByteChar))
                .or(Literal::unicode_char_parser().map(Literal::UnicodeChar))
                .or(Literal::bool_parser().map(Literal::Boolean))
                .or(just(Token::Null).to(Literal::Null))
            //.or(array_lit(literal).map(CtLiteral::Array))
        })
    }
}

#[derive(Debug, Clone)]
enum PtrArr {
    Arr(Option<Expr>),
    Ptr,
}

impl Type {
    fn builtin_parser<'a>() -> impl Parser<'a, Type> {
        filter(Token::is_ident_lit("sint8"))
            .to(Type::I8)
            .or(filter(Token::is_ident_lit("sint16")).to(Type::I16))
            .or(filter(Token::is_ident_lit("sint32")).to(Type::I32))
            .or(filter(Token::is_ident_lit("sint64")).to(Type::I64))
            .or(filter(Token::is_ident_lit("uint8")).to(Type::U8))
            .or(filter(Token::is_ident_lit("uint16")).to(Type::U16))
            .or(filter(Token::is_ident_lit("uint32")).to(Type::U32))
            .or(filter(Token::is_ident_lit("uint64")).to(Type::U64))
            .or(filter(Token::is_ident_lit("float32")).to(Type::F32))
            .or(filter(Token::is_ident_lit("float64")).to(Type::F64))
            .or(filter(Token::is_ident_lit("string")).to(Type::String))
            .or(filter(Token::is_ident_lit("bool")).to(Type::Bool))
            .or(filter(Token::is_ident_lit("null")).to(Type::Null))
    }

    fn parser<'a>() -> impl Parser<'a, Type> {
        let following = Expr::parser()
            .or_not()
            .delimited_by(just(Token::OpenBracket), just(Token::CloseBracket))
            .map(|e| PtrArr::Arr(e))
            .or(just(Token::Asterisk).to(PtrArr::Ptr))
            .repeated();

        Type::builtin_parser()
            .or(filter_map(Token::map_ident).map(|i| Type::Section(i)))
            .then(following)
            .foldl(|ty, follow| match follow {
                PtrArr::Arr(expr) => Type::Array(Box::new(ty), expr),
                PtrArr::Ptr => Type::Pointer(Box::new(ty)),
            })
    }
}

impl Condition {
    fn parser<'a>(
        ty_constraint: impl Parser<'a, TyConstraint> + Clone
    ) -> impl Parser<'a, Condition> {
        just(Token::Question)
            .ignore_then(Expr::parser().delimited_by(just(Token::OpenParen), just(Token::CloseParen)))
            .then_ignore(just(Token::LessThan))
            .then(ty_constraint.clone())
            .then_ignore(just(Token::GreaterThan))
            .then_ignore(just(Token::LessThan))
            .then(ty_constraint)
            .then_ignore(just(Token::GreaterThan))
            .map(|((condition, left), right)| Condition {
                condition,
                left: Box::new(left),
                right: Box::new(right),
            })
    }
}

impl UnOp {
    fn parser<'a>() -> impl Parser<'a, UnOp> {
        just(Token::Dash)
            .to(UnOp::Neg)
            .or(just(Token::Bang).to(UnOp::Inv))
    }
}

impl BinOp {
    fn product<'a>() -> impl Parser<'a, BinOp> {
        just(Token::Asterisk)
            .to(BinOp::Mul)
            .or(just(Token::Slash).to(BinOp::Div))
            .or(just(Token::Percent).to(BinOp::Rem))
    }

    fn sum<'a>() -> impl Parser<'a, BinOp> + Clone {
        just(Token::Plus)
            .to(BinOp::Add)
            .or(just(Token::Dash).to(BinOp::Sub))
    }

    fn cmp<'a>() -> impl Parser<'a, BinOp> + Clone {
        just(Token::EqEq)
            .to(BinOp::Is)
            .or(just(Token::LtEq).to(BinOp::Le))
            .or(just(Token::GtEq).to(BinOp::Ge))
            .or(just(Token::LessThan).to(BinOp::Lt))
            .or(just(Token::GreaterThan).to(BinOp::Gt))
    }

    fn and<'a>() -> impl Parser<'a, BinOp> + Clone {
        filter(Token::is_ident_lit("and")).to(BinOp::And)
    }

    fn or<'a>() -> impl Parser<'a, BinOp> + Clone {
        filter(Token::is_ident_lit("or")).to(BinOp::Or)
    }
}

impl Expr {
    fn parser<'a>() -> impl Parser<'a, Expr> {
        recursive(|expr| {
            let array = expr.clone().separated_by(just(Token::Comma))
                .delimited_by(just(Token::OpenBracket), just(Token::CloseBracket));

            let atom = filter_map(Token::map_ident)
                .map(Expr::Ident)
                .or(array.map(Expr::Array))
                .or(Literal::parser().map(Expr::Literal))
                .or(expr
                    .delimited_by(just(Token::OpenParen), just(Token::CloseParen))
                    .map(|e| Expr::Parens(Box::new(e))));

            let unary = UnOp::parser()
                .repeated()
                .then(atom)
                .foldr(|op, rhs| Expr::Unary(op, Box::new(rhs)));

            let precedence = [
                BinOp::product().boxed(),
                BinOp::sum().boxed(),
                BinOp::cmp().boxed(),
                BinOp::and().boxed(),
                BinOp::or().boxed(),
            ];

            let mut last = unary.boxed();

            for ops in precedence {
                last = last
                    .clone()
                    .then(ops.then(last).repeated())
                    .foldl(|lhs, (op, rhs)| Expr::Binary(Box::new(lhs), op, Box::new(rhs)))
                    .boxed();
            }

            last
        })
    }
}
