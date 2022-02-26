
use crate::ast::CtLiteral;

use std::str::FromStr;
use std::convert::TryFrom;
use std::iter::FromIterator;

use chumsky::prelude::*;
use chumsky::text::keyword;

fn predef_esc() -> impl Parser<char, char, Error = Simple<char>> + Clone {
    just("\\n").to('\n')
        .or(just("\\r").to('\r'))
        .or(just("\\t").to('\t'))
        .or(just("\\\\").to('\\'))
        .or(just("\\0").to('\0'))
        .or(just("\\\"").to('\"'))
        .or(just("\\'").to('\''))
}

fn byte_esc() -> impl Parser<char, char, Error = Simple<char>> + Clone {
    just("\\x")
        .ignore_then(
            filter(|c: &char| c.is_digit(16))
                .repeated()
                .exactly(2)
                .map(String::from_iter)
        )
        .map(|c| char::from(u8::from_str_radix(&c, 16).unwrap()))
}

fn unicode_esc() -> impl Parser<char, char, Error = Simple<char>> + Clone {
    just("\\u{")
        .ignore_then(
            filter(|c: &char| c.is_digit(16))
                .repeated()
                .at_least(1)
                .at_most(6)
                .map(String::from_iter)
        )
        .then_ignore(just('}'))
        // TODO: Error handle this, it's second unwrap is fallible
        .map(|c| char::from_u32(u32::from_str_radix(&c, 16).unwrap()).unwrap())
}

fn internal_str() -> impl Parser<char, Vec<char>, Error = Simple<char>> + Clone {
    let raw_str = none_of(['\"', '\\']);

    raw_str
        .or(predef_esc())
        .or(byte_esc())
        .or(unicode_esc())
        .repeated()
}

fn int_lit() -> impl Parser<char, i128, Error = Simple<char>> + Clone {
    let radix = |s: &'static str, r| just(s).ignore_then(text::digits(r));

    just('-')
        .or_not()
        .then(
        radix("0b", 2).map(|i: String| i128::from_str_radix(&i, 2).unwrap())
            .or(radix("0o", 8).map(|i| i128::from_str_radix(&i, 8).unwrap()))
            .or(radix("0x", 16).map(|i| i128::from_str_radix(&i, 16).unwrap()))
            .or(text::int(10).map(|i: String| i128::from_str_radix(&i, 10).unwrap()))
        )
        .map(|(neg, val)| if neg.is_some() {
            -val
        } else {
            val
        })
}

fn float_lit() -> impl Parser<char, f64, Error = Simple<char>> + Clone {
    just('-')
        .or_not()
        .then(
        filter(|c: &char| c.is_digit(10))
            .repeated()
            .map(String::from_iter)
            .then_ignore(just('.'))
            .then(filter(|c: &char| c.is_digit(10)).repeated().map(String::from_iter))
            .map(|(pre, post)| f64::from_str(&(pre + "." + &post)).unwrap())
        )
        .map(|(neg, val)| if neg.is_some() {
            -val
        } else {
            val
        })
}

fn byte_string() -> impl Parser<char, Vec<u8>, Error = Simple<char>> + Clone {
    filter(|c: &char| *c == '"')
        .ignore_then(internal_str())
        .then_ignore(just('"'))
        // TODO: Error handle non-byte chars instead of unwrap
        .map(|s|
            s.into_iter()
                .map(|c| u8::try_from(c as u32))
                .collect::<Result<_, _>>()
                .unwrap()
        )
}

fn unicode_string() -> impl Parser<char, String, Error = Simple<char>> + Clone {
    just("u\"")
        .ignore_then(internal_str())
        .then_ignore(just('"'))
        .map(String::from_iter)
}

fn regex_string() -> impl Parser<char, String, Error = Simple<char>> + Clone {
    just("r\"")
        .ignore_then(internal_str())
        .then_ignore(just('"'))
        .map(String::from_iter)
}

fn byte_char() -> impl Parser<char, u8, Error = Simple<char>> + Clone {
    just('\'')
        .ignore_then(
            none_of(['\\', '\''])
                .or(predef_esc())
                .or(byte_esc())
        )
        .then_ignore(just('\''))
        .map(|s| u8::try_from(s as u32).unwrap())
}

fn unicode_char() -> impl Parser<char, char, Error = Simple<char>> + Clone {
    just("u'")
        .ignore_then(
            none_of(['\\', '\''])
                .or(predef_esc())
                .or(byte_esc())
                .or(unicode_esc())
        )
        .then_ignore(just('\''))
}

fn bool_lit() -> impl Parser<char, bool, Error = Simple<char>> + Clone {
    keyword("true")
        .to(true)
        .or(keyword("false").to(false))
}

/*fn array_lit(literal: impl Parser<char, CtLiteral, Error = Simple<char>>) -> impl Parser<char, Vec<CtLiteral>, Error = Simple<char>> {
    just('[')
        .ignore_then(literal.separated_by(just(',').padded()))
        .then_ignore(just(']'))
}*/

pub fn literal() -> impl Parser<char, CtLiteral, Error = Simple<char>> + Clone {
    recursive(|_| {
        float_lit()
            .map(CtLiteral::Float)
            .or(int_lit().map(CtLiteral::Integer))
            .or(byte_string().map(CtLiteral::ByteString))
            .or(unicode_string().map(CtLiteral::UnicodeString))
            .or(regex_string().map(CtLiteral::Regex))
            .or(byte_char().map(CtLiteral::ByteChar))
            .or(unicode_char().map(CtLiteral::UnicodeChar))
            .or(bool_lit().map(CtLiteral::Boolean))
            //.or(array_lit(literal).map(CtLiteral::Array))
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_int() {
        assert_eq!(int_lit().parse("101"), Ok(101));
        assert_eq!(int_lit().parse("0b101"), Ok(5));
        assert_eq!(int_lit().parse("0o101"), Ok(65));
        assert_eq!(int_lit().parse("0x101"), Ok(257));
    }

    #[test]
    fn test_float() {
        assert_eq!(float_lit().parse("101."), Ok(101.));
        assert_eq!(float_lit().parse("1.5"), Ok(1.5));

        float_lit().parse("1").unwrap_err();
    }

    #[test]
    fn test_internal_str() {
        assert_eq!(
            internal_str().parse(""),
            Ok("".chars().collect()),
        );

        assert_eq!(
            internal_str().parse("abcdefg\\n\\x7B\\x7D"),
            Ok("abcdefg\n{}".chars().collect()),
        );
    }

    #[test]
    fn test_byte_str() {
        assert_eq!(
            byte_string().parse("\"abcdefg\""),
            Ok(b"abcdefg".to_vec()),
        );

        byte_string().parse("\"\\\"").unwrap_err();
        byte_string().parse("u\"\"").unwrap_err();
        byte_string().parse("r\"\"").unwrap_err();
    }

    #[test]
    fn test_unicode_str() {
        assert_eq!(
            unicode_string().parse("u\"abcdefg\""),
            Ok("abcdefg".to_owned()),
        );

        unicode_string().parse("u\"\\\"").unwrap_err();
        unicode_string().parse("\"\"").unwrap_err();
        unicode_string().parse("r\"\"").unwrap_err();
    }

    #[test]
    fn test_regex_str() {
        assert_eq!(
            regex_string().parse("r\"abcdefg\""),
            Ok("abcdefg".to_owned()),
        );

        regex_string().parse("r\"\\\"").unwrap_err();
        regex_string().parse("u\"\"").unwrap_err();
        regex_string().parse("\"\"").unwrap_err();
    }

    #[test]
    fn test_char() {
        assert_eq!(
            byte_char().parse("'a'"),
            Ok('a'),
        );
        assert_eq!(
            byte_char().parse("'\\u{30}'"),
            Ok('0'),
        );

        byte_char().parse("'12'").unwrap_err();
    }

    #[test]
    fn test_bool() {
        assert_eq!(bool_lit().parse("true"), Ok(true));
        assert_eq!(bool_lit().parse("false"), Ok(false));

        bool_lit().parse("papaya").unwrap_err();
    }

    /*#[test]
    fn test_array() {
        assert_eq!(
            array_lit(literal()).parse("[1, 2, 3]"),
            Ok(vec![CtLiteral::Integer(1), CtLiteral::Integer(2), CtLiteral::Integer(3)]),
        );
    }*/
}
