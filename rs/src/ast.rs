
// use pest::prec_climber::{Operator, PrecClimber, Assoc};

use std::convert::TryFrom;

use crate::prec::{Operator, Assoc, PrecClimber, Position};
use crate::parse::{Pair, Rule, Error as ParseError};

macro_rules! assert_rule {
    ($pair:ident, $rule:ident) => {
        if $pair.as_rule() != Rule::$rule {
            return Err(ParseError::InvalidRule(Rule::$rule, $pair.as_rule()));
        }
    }
}

#[derive(Debug)]
pub enum CtOp {
    Or,
    And,
    Is,

    LogOr,
    LogAnd,

    Le,
    Ge,
    Lt,
    Gt,

    Add,
    Sub,
    Mul,
    Div,
    Rem,

    Not,
    Neg,

    Call(Vec<CtExpr>),
    Index(Vec<CtExpr>),

    Dot,
}

impl TryFrom<Pair<'_>> for CtOp {
    type Error = ParseError;

    fn try_from(pair: Pair<'_>) -> Result<Self, Self::Error> {
        Ok(match pair.as_rule() {
            Rule::op_or => CtOp::Or,
            Rule::op_and => CtOp::And,
            Rule::op_is => CtOp::Is,

            Rule::op_log_or => CtOp::LogOr,
            Rule::op_log_and => CtOp::LogAnd,

            Rule::op_le => CtOp::Le,
            Rule::op_ge => CtOp::Ge,
            Rule::op_lt => CtOp::Lt,
            Rule::op_gt => CtOp::Gt,

            Rule::op_add => CtOp::Add,
            Rule::op_sub => CtOp::Sub,
            Rule::op_mul => CtOp::Mul,
            Rule::op_div => CtOp::Div,
            Rule::op_rem => CtOp::Rem,

            Rule::op_neg => CtOp::Neg,
            Rule::op_not => CtOp::Not,

            Rule::op_call => CtOp::Call(
                pair.into_inner()
                    .next()
                    .map(|p| p.into_inner()
                        .map(CtExpr::try_from)
                        .collect::<Result<_, _>>()
                    )
                    .transpose()?
                    .unwrap_or_default()
            ),
            Rule::op_index => CtOp::Index(
                pair.into_inner()
                    .next()
                    .unwrap()
                    .into_inner()
                    .map(CtExpr::try_from)
                    .collect::<Result<_, _>>()?
            ),

            Rule::op_dot => CtOp::Dot,
            rule => return Err(ParseError::InvalidRule(Rule::expr_op, rule)),
        })
    }
}

#[derive(Debug)]
pub enum CtExprTarget {
    Literal(CtLiteral),
    Ident(String),
    Expr(Box<CtExpr>),
}

impl TryFrom<Pair<'_>> for CtExprTarget {
    type Error = ParseError;

    fn try_from(pair: Pair<'_>) -> Result<Self, Self::Error> {
        assert_rule!(pair, expr_target);

        let res = pair.into_inner().next().unwrap();
        match res.as_rule() {
            Rule::literal => Ok(CtExprTarget::Literal(CtLiteral::try_from(res)?)),
            Rule::ident => Ok(CtExprTarget::Ident(res.as_str().to_string())),
            Rule::expr => Ok(CtExprTarget::Expr(Box::new(CtExpr::try_from(res)?))),
            _ => unreachable!()
        }
    }
}

#[derive(Debug)]
pub enum CtExpr {
    Leaf(CtExprTarget),
    PrefixOp {
        op: CtOp,
        right: Box<CtExpr>,
    },
    PostfixOp {
        left: Box<CtExpr>,
        op: CtOp,
    },
    Op {
        left: Box<CtExpr>,
        op: CtOp,
        right: Box<CtExpr>,
    }
}

impl TryFrom<Pair<'_>> for CtExpr {
    type Error = ParseError;

    fn try_from(pair: Pair<'_>) -> Result<Self, Self::Error> {
        assert_rule!(pair, expr);
        let climber = PrecClimber::new(vec![
            Operator::new(Rule::op_or),
            Operator::new(Rule::op_and),
            Operator::new(Rule::op_is),
            // Logical Operators
            Operator::new(Rule::op_log_or),
            Operator::new(Rule::op_log_and),
            // Comparison
            Operator::new(Rule::op_lt) |
                Operator::new(Rule::op_gt) |
                Operator::new(Rule::op_le) |
                Operator::new(Rule::op_ge),
            // Mathematics
            Operator::new(Rule::op_add) |
                Operator::new(Rule::op_sub),
            Operator::new(Rule::op_mul) |
                Operator::new(Rule::op_div) |
                Operator::new(Rule::op_rem),
            Operator::new_pos(Rule::op_neg, Position::Prefix) |
                Operator::new_pos(Rule::op_not, Position::Prefix),
            // Indexing/Calls
            Operator::new_pos(Rule::op_call, Position::Suffix) |
                Operator::new_pos(Rule::op_index, Position::Suffix),
            // Property access
            Operator::new(Rule::op_dot),
        ]);

        climber.climb(
            pair.into_inner(),
            |pair| {
                Ok(CtExpr::Leaf(CtExprTarget::try_from(pair)?))
            },
        |left, op, right| {
                let left = left?;
                let right = right?;
                Ok(CtExpr::Op {
                    left: Box::new(left),
                    op: CtOp::try_from(op)?,
                    right: Box::new(right),
                })
            },
            |op, right| {
                let right = right?;
                Ok(CtExpr::PrefixOp {
                    op: CtOp::try_from(op)?,
                    right: Box::new(right),
                })
            },
            |left, op| {
                let left = left?;
                Ok(CtExpr::PostfixOp {
                    left: Box::new(left),
                    op: CtOp::try_from(op)?,
                })
            },
        )
    }
}

#[derive(Debug)]
pub enum CtLiteral {
    Integer(i128),
    Float(f64),
    ByteString(Vec<u8>),
    UnicodeString(String),
    Regex(String),
    Char(char),
    Boolean(bool),
    Array(Vec<CtLiteral>),
}

impl TryFrom<Pair<'_>> for CtLiteral {
    type Error = ParseError;

    fn try_from(pair: Pair<'_>) -> Result<Self, Self::Error> {
        assert_rule!(pair, literal);
        let res = pair.into_inner().next().unwrap();

        Ok(match res.as_rule() {
            Rule::float_literal => {
                let res = res.as_str();
                let res = &res[..(res.len() - 1)];
                CtLiteral::Float(res.parse().unwrap())
            },
            Rule::int_literal => {
                let res = res.as_str();
                let (res, radix) = if res.starts_with("0x") {
                    (&res[2..], 16)
                } else if res.starts_with("0o") {
                    (&res[2..], 8)
                } else if res.starts_with("0b") {
                    (&res[2..], 2)
                } else {
                    (res, 10)
                };
                CtLiteral::Integer(i128::from_str_radix(res, radix).unwrap())
            },
            Rule::string_literal => {
                let res = res.as_str();
                // TODO: Parse escapes
                if res.starts_with('u') {
                    CtLiteral::UnicodeString(res.to_string())
                } else if res.starts_with('r') {
                    CtLiteral::ByteString(res.as_bytes().to_owned())
                } else {
                    CtLiteral::ByteString(res.as_bytes().to_owned())
                }
            },
            Rule::char_literal => {
                let res = res.as_str();
                // TODO: Parse escapes
                if res.starts_with('u') {
                    CtLiteral::Char(res[1..].trim_matches('\'').chars().nth(0).unwrap())
                } else {
                    CtLiteral::Char(res.trim_matches('\'').chars().nth(0).unwrap())
                }
            },
            Rule::boolean_literal => {
                if res.as_str() == "true" {
                    CtLiteral::Boolean(true)
                } else {
                    CtLiteral::Boolean(false)
                }
            },
            Rule::array_literal => {
                let vals = res.into_inner()
                    .map(CtLiteral::try_from)
                    .collect::<Result<_, _>>()?;
                CtLiteral::Array(vals)
            },
            _ => unreachable!()
        })
    }
}

#[derive(Debug)]
pub enum BuiltinType {
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    F32,
    F64,
    String,
    Bool,
    Null,
    Array(Box<CtType>, Option<CtExpr>),
    Pointer(Box<CtType>),
}

#[derive(Debug)]
pub enum CtType {
    Builtin(BuiltinType),
    Section(String),
}

fn parse_ty_name(pair: Pair<'_>) -> CtType {
    let res = pair.into_inner().next().unwrap();
    match res.as_rule() {
        Rule::builtin => CtType::Builtin(match res.as_str() {
            "sint8" => BuiltinType::I8,
            "sint16" => BuiltinType::I16,
            "sint32" => BuiltinType::I32,
            "sint64" => BuiltinType::I64,
            "uint8" => BuiltinType::U8,
            "uint16" => BuiltinType::U16,
            "uint32" => BuiltinType::U32,
            "uint64" => BuiltinType::U64,
            "float32" => BuiltinType::F32,
            "float64" => BuiltinType::F64,
            "string" => BuiltinType::String,
            "bool" => BuiltinType::Bool,
            "null" => BuiltinType::Null,
            _ => unreachable!(),
        }),
        Rule::ident => CtType::Section(res.as_str().to_string()),
        _ => unreachable!(),
    }
}

impl TryFrom<Pair<'_>> for CtType {
    type Error = ParseError;

    fn try_from(pair: Pair<'_>) -> Result<Self, Self::Error> {
        assert_rule!(pair, ty);

        let mut res = pair.into_inner();
        let mut out = parse_ty_name(res.next().unwrap());
        for remaining in res {
            match remaining.as_rule() {
                Rule::pointer => {
                    out = CtType::Builtin(BuiltinType::Pointer(Box::new(out)));
                },
                Rule::array => {
                    let expr = remaining.into_inner().next()
                        .map(CtExpr::try_from)
                        .transpose()?;
                    out = CtType::Builtin(BuiltinType::Array(Box::new(out), expr))
                },
                _ => unreachable!(),
            }
        }
        Ok(out)
    }
}

#[derive(Debug)]
pub struct CtCondition {
    condition: CtExpr,
    left: CtTyConstraint,
    right: CtTyConstraint,
}

impl TryFrom<Pair<'_>> for CtCondition {
    type Error = ParseError;

    fn try_from(pair: Pair<'_>) -> Result<Self, Self::Error> {
        assert_rule!(pair, conditional);
        let mut res = pair.into_inner();
        Ok(CtCondition {
            condition: CtExpr::try_from(res.next().unwrap())?,
            left: CtTyConstraint::try_from(res.next().unwrap())?,
            right: CtTyConstraint::try_from(res.next().unwrap())?,
        })
    }
}

#[derive(Debug)]
pub enum CtTyConstraint {
    Literal(CtLiteral),
    Type(CtType),
    Union(Vec<CtTyConstraint>),
    Conditional(Box<CtCondition>),
    Global(CtType),
}

fn parse_constraint_sub(pair: Pair<'_>) -> Result<CtTyConstraint, ParseError> {
    assert_rule!(pair, constraint_sub);
    let res = pair.into_inner().next().unwrap();

    Ok(match res.as_rule() {
        Rule::literal => CtTyConstraint::Literal(CtLiteral::try_from(res)?),
        Rule::ty => CtTyConstraint::Type(CtType::try_from(res)?),
        Rule::conditional => CtTyConstraint::Conditional(Box::new(CtCondition::try_from(res)?)),
        Rule::global => CtTyConstraint::Global(CtType::try_from(res.into_inner().next().unwrap())?),
        _ => unreachable!(),
    })
}

impl TryFrom<Pair<'_>> for CtTyConstraint {
    type Error = ParseError;

    fn try_from(pair: Pair<'_>) -> Result<Self, Self::Error> {
        assert_rule!(pair, ty_constraint);
        let mut res = pair.into_inner();

        let out = parse_constraint_sub(res.next().unwrap())?;

        if res.peek().is_none() {
            Ok(out)
        } else {
            let mut union = vec![out];

            while let Some(item) = res.next() {
                union.push(parse_constraint_sub(item)?)
            }

            Ok(CtTyConstraint::Union(union))
        }
    }
}

#[derive(Debug)]
pub struct CtValConstraint {
    expr: CtExpr,
}

impl TryFrom<Pair<'_>> for CtValConstraint {
    type Error = ParseError;

    fn try_from(pair: Pair<'_>) -> Result<Self, Self::Error> {
        assert_rule!(pair, val_constraint);
        Ok(CtValConstraint {
            expr: CtExpr::try_from(pair.into_inner().next().unwrap())?
        })
    }
}

#[derive(Debug)]
pub struct CtConstraint {
    pub name: Option<String>,
    pub ty_constraint: CtTyConstraint,
    pub val_constraint: Option<CtValConstraint>
}

impl TryFrom<Pair<'_>> for CtConstraint {
    type Error = ParseError;

    fn try_from(pair: Pair<'_>) -> Result<Self, Self::Error> {
        assert_rule!(pair, name_constraint);
        let mut res = pair.into_inner();

        let name = if let Rule::ident = res.peek().unwrap().as_rule() {
            Some(res.next().unwrap().as_str().to_string())
        } else {
            None
        };

        let ty_constraint = CtTyConstraint::try_from(res.next().unwrap())?;

        let val_constraint = res.next()
            .map(CtValConstraint::try_from)
            .transpose()?;

        Ok(CtConstraint {
            name,
            ty_constraint,
            val_constraint,
        })
    }
}

#[derive(Debug)]
pub enum SectionKind {
    Section,
    File,
}

#[derive(Debug)]
pub struct CtSection {
    pub kind: SectionKind,
    pub name: String,
    pub constraints: Vec<CtConstraint>,
}

impl TryFrom<Pair<'_>> for CtSection {
    type Error = ParseError;

    fn try_from(pair: Pair<'_>) -> Result<Self, Self::Error> {
        let kind = match pair.as_rule() {
            Rule::section => SectionKind::Section,
            Rule::file => SectionKind::File,
            rule => return Err(ParseError::InvalidRule(Rule::section, rule))
        };

        let mut res = pair.into_inner();

        Ok(CtSection {
            kind,
            name: res.next().unwrap().as_str().to_string(),
            constraints: res.next().unwrap().into_inner().map(CtConstraint::try_from).collect::<Result<_, _>>()?
        })
    }
}

#[derive(Debug)]
pub enum CtTypeDecl {
    Rule(CtType, String),
    Alias(String, CtTyConstraint, Option<String>)
}

impl TryFrom<Pair<'_>> for CtTypeDecl {
    type Error = ParseError;

    fn try_from(pair: Pair<'_>) -> Result<Self, Self::Error> {
        assert_rule!(pair, type_decl);
        let res = pair.into_inner().next().unwrap();

        match res.as_rule() {
            Rule::ty_rule => {
                let mut res = res.into_inner();
                Ok(CtTypeDecl::Rule(CtType::try_from(res.next().unwrap())?, res.next().unwrap().as_str().to_string()))
            },
            Rule::ty_alias => {
                let mut res = res.into_inner();
                let name = res.next().unwrap().as_str().to_string();
                let ty = CtTyConstraint::try_from(res.next().unwrap())?;
                let option = res.next().map(|p| p.as_str().to_string());

                Ok(CtTypeDecl::Alias(name, ty, option))
            },
            _ => unreachable!()
        }
    }
}

#[derive(Debug)]
pub struct CtEncoding {
    pub encoding: String,
}

impl TryFrom<Pair<'_>> for CtEncoding {
    type Error = ParseError;

    fn try_from(pair: Pair<'_>) -> Result<Self, Self::Error> {
        assert_rule!(pair, encoding);
        Ok(CtEncoding {
            encoding: pair.into_inner().next().unwrap().as_str().to_string()
        })
    }
}

#[derive(Debug)]
pub struct CtAst {
    pub encoding: Option<CtEncoding>,
    pub type_decls: Vec<CtTypeDecl>,
    pub sections: Vec<CtSection>,
}

impl TryFrom<Pair<'_>> for CtAst {
    type Error = ParseError;

    fn try_from(pair: Pair<'_>) -> Result<Self, Self::Error> {
        assert_rule!(pair, tl);

        let mut ast = CtAst {
            encoding: None,
            type_decls: vec![],
            sections: vec![],
        };

        for i in pair.into_inner() {
            match i.as_rule() {
                Rule::encoding => {
                    if ast.encoding.is_some() {
                        return Err(ParseError::MultipleEncodings)
                    }

                    ast.encoding = Some(CtEncoding::try_from(i)?)
                }
                Rule::type_decl => {
                    ast.type_decls.push(CtTypeDecl::try_from(i)?)
                }
                Rule::section | Rule::file => {
                    ast.sections.push(CtSection::try_from(i)?)
                }
                Rule::EOI => {}
                _ => unreachable!()
            }
        }

        Ok(ast)
    }
}
