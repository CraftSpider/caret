
use crate::ast::*;

use std::fs;
use std::error::Error;

use pest::Parser;
use pest_derive::Parser;
use pest::iterators;

type Pairs<'a> = iterators::Pairs<'a, Rule>;
type Pair<'a> = iterators::Pair<'a, Rule>;

type Result<T> = std::result::Result<T, Box<dyn Error>>;

#[derive(Parser)]
#[grammar = "caret.pest"]
struct CaretParser;

fn parse_encoding(pair: Pair) -> CtEncoding {
    CtEncoding {
        encoding: pair.as_str().to_string()
    }
}

fn parse_ty_rule(pair: Pair) -> CtTypeDecl {
    let mut res: Vec<Pair> = pair.into_inner().collect();

    CtTypeDecl::Rule(parse_ty(res.remove(0)), res[0].as_str().to_string())
}

fn parse_ty_alias(pair: Pair) -> CtTypeDecl {
    let mut res: Vec<Pair> = pair.into_inner().collect();

    CtTypeDecl::Alias(res[0].as_str().to_string(), parse_constraint(res.remove(1)), res[1].as_str().to_string())
}

fn parse_type_decl(pair: Pair) -> CtTypeDecl {
    let res: Pair = pair.into_inner().collect::<Vec<_>>().remove(0);

    match res.as_rule() {
        Rule::ty_rule => parse_ty_rule(res),
        Rule::ty_alias => parse_ty_alias(res),
        _ => unreachable!()
    }
}

fn parse_section(kind: SectionKind, pair: Pair) -> CtSection {
    let mut res: Vec<Pair> = pair.into_inner().collect();

    CtSection {
        kind,
        name: res.remove(0).as_str().to_string(),
        constraints: res.into_iter().map(|pair| parse_constraint(pair)).collect()
    }
}

fn parse_ty(pair: Pair) -> CtType {
    todo!()
}

fn parse_constraint(pair: Pair) -> CtConstraint {
    todo!()
}

fn parse_tl(pair: Pair) -> Result<CtAst> {
    let mut ast = CtAst {
        encoding: None,
        type_decls: vec![],
        sections: vec![],
    };

    for i in pair.into_inner() {
        match i.as_rule() {
            Rule::encoding => {
                if ast.encoding.is_some() {
                    return Err("Cannot specify multiple encodings".into())
                }

                ast.encoding = Some(parse_encoding(i))
            }
            Rule::type_decl => {
                ast.type_decls.push(parse_type_decl(i))
            }
            Rule::section => {
                ast.sections.push(parse_section(SectionKind::Section, i))
            }
            Rule::file => {
                ast.sections.push(parse_section(SectionKind::File, i))
            }
            Rule::EOI => {}
            _ => unreachable!()
        }
    }

    Ok(ast)
}

pub fn parse(filename: &str) -> Result<CtAst> {
    let contents = fs::read_to_string(filename)?;

    let mut input: Vec<_> = CaretParser::parse(Rule::tl, &contents)?.collect();
    assert_eq!(input.len(), 1);

    parse_tl(input.remove(0))
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