use crate::ast::Literal;
use crate::compile::new_stream;

use chumsky::Parser;

#[test]
fn test_int() {
    assert_eq!(
        Literal::int_parser().parse(new_stream("101", "")),
        Ok(101)
    );
    assert_eq!(
        Literal::int_parser().parse(new_stream("0b101", "")),
        Ok(5)
    );
    assert_eq!(
        Literal::int_parser().parse(new_stream("0o101", "")),
        Ok(65)
    );
    assert_eq!(
        Literal::int_parser().parse(new_stream("0x101", "")),
        Ok(257)
    );
}

#[test]
fn test_float() {
    assert_eq!(
        Literal::float_parser().parse(new_stream("101.", "")),
        Ok(101.)
    );

    assert_eq!(
        Literal::float_parser().parse(new_stream("1.5", "")),
        Ok(1.5)
    );
}

#[test]
fn test_byte_str() {
    assert_eq!(
        Literal::byte_str_parser().parse(new_stream("\"abcdefg\"", "")),
        Ok(b"abcdefg".to_vec()),
    );

    Literal::byte_str_parser()
        .parse(new_stream("u\"\"", "", ))
        .unwrap_err();

    Literal::byte_str_parser()
        .parse(new_stream("r\"\"", "", ))
        .unwrap_err();
}

#[test]
fn test_unicode_str() {
    assert_eq!(
        Literal::unicode_str_parser().parse(new_stream(
            "u\"abcdefg\"",
            ""
        )),
        Ok("abcdefg".to_owned()),
    );

    Literal::unicode_str_parser()
        .parse(new_stream("\"\"", "", ))
        .unwrap_err();

    Literal::unicode_str_parser()
        .parse(new_stream("r\"\"", "", ))
        .unwrap_err();
}

#[test]
fn test_regex_str() {
    assert_eq!(
        Literal::regex_str_parser().parse(new_stream("r\"abcdefg\"", "")),
        Ok("abcdefg".to_owned()),
    );

    Literal::regex_str_parser()
        .parse(new_stream("u\"\"", "", ))
        .unwrap_err();

    Literal::regex_str_parser()
        .parse(new_stream("\"\"", "", ))
        .unwrap_err();
}

#[test]
fn test_char() {
    assert_eq!(
        Literal::byte_char_parser().parse(new_stream(
            "'a'",
            ""
        )),
        Ok(b'a'),
    );
    assert_eq!(
        Literal::byte_char_parser().parse(new_stream(
            "'\\u{30}'",
            ""
        )),
        Ok(b'0'),
    );
}

#[test]
fn test_bool() {
    assert_eq!(Literal::bool_parser().parse(new_stream("true", "")), Ok(true));
    assert_eq!(Literal::bool_parser().parse(new_stream("false", "")), Ok(false));
}

/*#[test]
fn test_array() {
    assert_eq!(
        Literal::array_parser(literal()).parse("[1, 2, 3]"),
        Ok(vec![CtLiteral::Integer(1), CtLiteral::Integer(2), CtLiteral::Integer(3)]),
    );
}*/
