use crate::ast::{TyConstraint, Type, TypeDecl};
use crate::compile::new_stream;

use chumsky::Parser;

#[test]
fn test_type_alias() {
    assert_eq!(
        TypeDecl::parser().parse(new_stream("type Foo = Bar", "")),
        Ok(TypeDecl::Alias(
            String::from("Foo"),
            TyConstraint::Type(Type::Section(String::from("Bar"))),
            String::new()
        )),
    );
    assert_eq!(
        TypeDecl::parser().parse(new_stream("type Foo = Bar quoted", "")),
        Ok(TypeDecl::Alias(
            String::from("Foo"),
            TyConstraint::Type(Type::Section(String::from("Bar"))),
            String::from("quoted")
        )),
    );
    assert_eq!(
        TypeDecl::parser().parse(new_stream("type Foo = Bar\nquoted", "")),
        Ok(TypeDecl::Alias(
            String::from("Foo"),
            TyConstraint::Type(Type::Section(String::from("Bar"))),
            String::new()
        )),
    );
}
