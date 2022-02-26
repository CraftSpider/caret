
use crate::ast::{CtEncoding, CtTypeDecl, CtSection, CtTopLevel, SectionKind};
use crate::parse::{constraint, ty};

use chumsky::prelude::*;

fn encoding() -> impl Parser<char, CtEncoding, Error = Simple<char>> {
    just("encoding")
        .padded()
        .ignore_then(text::ident().padded())
        .map(|i: String| CtEncoding { encoding: i.to_string() })
}

fn ty_alias() -> impl Parser<char, CtTypeDecl, Error = Simple<char>> {
    just("type")
        .padded()
        .ignore_then(text::ident().padded())
        .then_ignore(just('=').padded())
        .then(constraint::ty_constraint())
        .then(
            filter(|c: &char| c.is_whitespace() && !['\n', '\r'].contains(c))
                .repeated()
                .ignore_then(text::ident())
                .or_not()
        )
        .map(|((ty, constraint), extra)| CtTypeDecl::Alias(ty.to_string(), constraint, extra.unwrap_or(String::new())))
}

fn ty_decl() -> impl Parser<char, CtTypeDecl, Error = Simple<char>> {
    just("type")
        .padded()
        .ignore_then(ty::ty().padded())
        .then(text::ident().padded())
        .map(|(ty, m)| CtTypeDecl::Rule(ty, m))
}

fn section() -> impl Parser<char, CtSection, Error = Simple<char>> {
    just("section")
        .padded()
        .ignore_then(text::ident().padded())
        .then(constraint::constraint().padded().repeated().delimited_by('{', '}'))
        .map(|(name, constraints)| {
            CtSection {
                kind: SectionKind::Section,
                name: name.to_owned(),
                constraints,
            }
        })
}

fn file() -> impl Parser<char, CtSection, Error = Simple<char>> {
    just("file")
        .padded()
        .ignore_then(text::ident().padded())
        .then(constraint::constraint().padded().repeated().delimited_by('{', '}'))
        .map(|(name, constraints)| {
            CtSection {
                kind: SectionKind::File,
                name: name.to_owned(),
                constraints,
            }
        })
}

pub fn top_level() -> impl Parser<char, CtTopLevel, Error = Simple<char>> {
    encoding().map(|e| CtTopLevel::Encoding(e))
        .or(ty_alias().map(|ta| CtTopLevel::Type(ta)))
        .or(ty_decl().map(|td| CtTopLevel::Type(td)))
        .or(section().map(|s| CtTopLevel::Section(s)))
        .or(file().map(|f| CtTopLevel::File(f)))
}

#[cfg(test)]
mod tests {
    use crate::ast::{CtTyConstraint, CtType};
    use super::*;

    #[test]
    fn test_type_alias() {
        assert_eq!(
            ty_alias().parse("type Foo = Bar"),
            Ok(CtTypeDecl::Alias(
                String::from("Foo"),
                CtTyConstraint::Type(CtType::Section(String::from("Bar"))),
                String::new()
            )),
        );
        assert_eq!(
            ty_alias().parse("type Foo = Bar quoted"),
            Ok(CtTypeDecl::Alias(
                String::from("Foo"),
                CtTyConstraint::Type(CtType::Section(String::from("Bar"))),
                String::from("quoted")
            )),
        );
        assert_eq!(
            ty_alias().parse("type Foo = Bar\nquoted"),
            Ok(CtTypeDecl::Alias(
                String::from("Foo"),
                CtTyConstraint::Type(CtType::Section(String::from("Bar"))),
                String::new()
            )),
        );
    }
}
