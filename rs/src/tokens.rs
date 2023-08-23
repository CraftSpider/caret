use crate::compile::parse::Error;
use crate::span::Span;

use std::convert::TryFrom;
use std::fmt;
use std::marker::PhantomData;

use chumsky::Error as _;
use logos::{Lexer, Logos};

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct RawInt<'a> {
    pub(crate) neg: bool,
    pub(crate) base: Option<&'a str>,
    pub(crate) val: &'a str,
    pub(crate) suffix: Option<&'a str>,
}

impl<'source> RawInt<'source> {
    fn handle(lex: &mut Lexer<'source, Token<'source>>) -> Self {
        let slice = lex.slice();
        let (neg, slice) = match slice.strip_prefix('-') {
            Some(slice) => (true, slice),
            None => (false, slice),
        };

        let (base, slice) = match *slice
            .split_inclusive(|x| ['b', 'x', 'o'].contains(&x))
            .collect::<Vec<_>>()
        {
            [base, val] => (Some(base), val),
            _ => (None, slice),
        };

        let (suffix, slice) = match *slice
            .split_inclusive(|x| ['i', 'u'].contains(&x))
            .collect::<Vec<_>>()
        {
            [val, suffix] => (Some(suffix), val),
            _ => (None, slice),
        };

        RawInt {
            neg,
            base,
            val: slice,
            suffix,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct RawFloat<'a> {
    pub(crate) neg: bool,
    pub(crate) val: &'a str,
    pub(crate) suffix: Option<&'a str>,
}

impl<'source> RawFloat<'source> {
    fn handle(lex: &mut Lexer<'source, Token<'source>>) -> Self {
        let slice = lex.slice();
        let (neg, slice) = match slice.strip_prefix('-') {
            Some(slice) => (true, slice),
            None => (false, slice),
        };
        let (suffix, slice) = match slice.find('f').map(|f| slice.split_at(f)) {
            Some((val, suffix)) => (Some(suffix), val),
            None => (None, slice),
        };

        RawFloat {
            neg,
            val: slice,
            suffix,
        }
    }
}

fn handle_plain<'source>(lex: &mut Lexer<'source, CharToken<'source>>) -> char {
    let slice = lex.slice();
    slice.chars().last().unwrap()
}

fn handle_byte<'source>(lex: &mut Lexer<'source, CharToken<'source>>) -> u8 {
    let slice = lex.slice();
    u8::from_str_radix(&slice[1..], 16)
        .unwrap()
}

fn handle_unicode<'source>(lex: &mut Lexer<'source, CharToken<'source>>) -> u32 {
    let slice = lex.slice();
    u32::from_str_radix(&slice[3..slice.len() - 1], 16)
        .unwrap()
}

#[derive(Logos, Clone, Debug, Eq, PartialEq, Hash)]
pub enum CharToken<'a> {
    #[regex(".", |lex| lex.slice().chars().next().unwrap())]
    Raw(char),
    #[regex(r#"\\[nrt\\0"']"#, handle_plain)]
    PlainEscape(char),
    #[regex(r#"\\x[0-9a-fA-F][0-9a-fA-F]"#, handle_byte)]
    ByteEscape(u8),
    #[regex(r#"\\u\{[0-9a-fA-F][0-9a-fA-F]?[0-9a-fA-F]?[0-9a-fA-F]?\}"#, handle_unicode)]
    UnicodeEscape(u32),

    #[error]
    Error,

    __Phantom(PhantomData<&'a ()>),
}

impl CharToken<'_> {
    pub(crate) fn span_of<'a>(&self, _str: Span<'a>) -> Span<'a> {
        todo!()
    }

    pub(crate) fn to_char(&self) -> Result<char, u32> {
        Ok(match self {
            CharToken::Raw(c) => *c,
            CharToken::PlainEscape(c) => match c {
                'n' => '\n',
                'r' => '\r',
                't' => '\t',
                '0' => '\0',
                // \ " or '
                c => *c,
            }
            CharToken::ByteEscape(b) => *b as char,
            CharToken::UnicodeEscape(i) => return char::try_from(*i)
                .map_err(|_| *i),
            _ => return Err(0),
        })
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct RawString<'a> {
    pub(crate) prefix: Option<&'a str>,
    pub(crate) val: Vec<CharToken<'a>>,
}

impl<'source> RawString<'source> {
    fn handle(lex: &mut Lexer<'source, Token<'source>>) -> Self {
        let slice = lex.slice();

        let (prefix, val) = match slice.split_once('"') {
            Some(("", val)) => (None, &val[..val.len() - 1]),
            Some((prefix, val)) => (Some(prefix), &val[..val.len() - 1]),
            None => unreachable!(),
        };

        let val = Lexer::new(val)
            .collect();

        RawString { prefix, val }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct RawChar<'a> {
    pub(crate) prefix: Option<&'a str>,
    pub(crate) val: CharToken<'a>,
}

impl<'source> RawChar<'source> {
    fn handle(lex: &mut Lexer<'source, Token<'source>>) -> Result<Self, Token<'source>> {
        let slice = lex.slice();
        let (prefix, val) = match slice.split_once('\'') {
            Some(("", val)) => (None, &val[..val.len() - 1]),
            Some((prefix, val)) => (Some(prefix), &val[..val.len() - 1]),
            None => unreachable!(),
        };

        let mut tokens = CharToken::lexer(val)
            .collect::<Vec<_>>();

        if tokens.len() != 1 {
            Err(Token::ERROR)
        } else {
            Ok(RawChar {
                prefix,
                val: tokens.remove(0),
            })
        }
    }
}

#[derive(Logos, Clone, Debug, Eq, PartialEq, Hash)]
pub enum Token<'a> {
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice())]
    Ident(&'a str),

    #[regex(
        r"-?(0x[0-9a-fA-F]+|0b[01]+|0o[0-7]+|[0-9]+)((u|i)[0-9]+)?",
        RawInt::handle
    )]
    Int(RawInt<'a>),

    #[regex(r"-?[0-9]+\.[0-9]*(f[0-9]+)?", RawFloat::handle)]
    Float(RawFloat<'a>),

    #[regex(r#"(u|r)?"([^\\"]|\\.)*""#, RawString::handle)]
    String(RawString<'a>),

    #[regex(r#"u?'([^\\']|\\.)*'"#, RawChar::handle)]
    Char(RawChar<'a>),

    #[token("true")]
    True,
    #[token("false")]
    False,
    #[token("null")]
    Null,

    #[token("`")]
    Tick,
    #[token("~")]
    Tilde,
    #[token("!")]
    Bang,
    #[token("@")]
    At,
    #[token("#")]
    Hash,
    #[token("$")]
    Dollar,
    #[token("%")]
    Percent,
    #[token("^")]
    Caret,
    #[token("&")]
    Ampersand,
    #[token("*")]
    Asterisk,
    #[token("(")]
    OpenParen,
    #[token(")")]
    CloseParen,
    #[token("-")]
    Dash,
    #[token("=")]
    Eq,
    #[token("+")]
    Plus,
    #[token("[")]
    OpenBracket,
    #[token("]")]
    CloseBracket,
    #[token("{")]
    OpenBrace,
    #[token("}")]
    CloseBrace,
    #[token("|")]
    Pipe,
    #[token(";")]
    SemiColon,
    #[token(":")]
    Colon,
    #[token(",")]
    Comma,
    #[token("<")]
    LessThan,
    #[token(".")]
    Dot,
    #[token(">")]
    GreaterThan,
    #[token("/")]
    Slash,
    #[token("?")]
    Question,

    #[token("<=")]
    LtEq,
    #[token(">=")]
    GtEq,
    #[token("==")]
    EqEq,
    #[token("->")]
    RightArrow,

    #[token("\n")]
    Newline,

    #[regex(r"//.*\n", |lex| lex.slice())]
    Comment(&'a str),

    #[regex(r"[ \t\f\r]", logos::skip)]
    #[error]
    Error,
}

impl fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let display = match self {
            Token::Ident("...") => "<identifier>",

            Token::Int(RawInt { val: "...", .. }) => "<integer value>",
            Token::Float(RawFloat { val: "...", .. }) => "<float value>",

            Token::String(RawString {
                prefix: None,
                val,
                ..
            }) if val == &[CharToken::Error] => "<byte string value>",
            Token::String(RawString {
                prefix: Some("u"),
                val,
                ..
            }) if val == &[CharToken::Error] => "<unicode string value>",
            Token::String(RawString {
                prefix: Some("r"),
                val,
                ..
            }) if val == &[CharToken::Error] => "<regex string value>",

            Token::Char(RawChar {
                prefix: None,
                val: CharToken::Error,
                ..
            }) => "<byte char value>",
            Token::Char(RawChar {
                prefix: Some("u"),
                val: CharToken::Error,
                ..
            }) => "<unicode char value>",

            Token::True => "true",
            Token::False => "false",
            Token::Null => "null",

            Token::Bang => "!",
            Token::Percent => "%",
            Token::Asterisk => "*",
            Token::OpenParen => "(",
            Token::CloseParen => ")",
            Token::Dash => "-",
            Token::Eq => "=",
            Token::Plus => "+",
            Token::OpenBracket => "[",
            Token::OpenBrace => "{",
            Token::CloseBracket => "]",
            Token::CloseBrace => "}",
            Token::Colon => ":",
            Token::LessThan => "<",
            Token::GreaterThan => ">",
            Token::Slash => "/",

            Token::GtEq => ">=",
            Token::LtEq => "<=",
            Token::EqEq => "==",

            _ => return write!(f, "{:?}", self),
        };

        write!(f, "{}", display)
    }
}

impl<'a> Token<'a> {
    pub fn expect_int() -> Token<'a> {
        Token::Int(RawInt {
            neg: false,
            base: None,
            val: "...",
            suffix: None,
        })
    }

    pub fn expect_float() -> Token<'a> {
        Token::Float(RawFloat {
            neg: false,
            val: "...",
            suffix: None,
        })
    }

    pub fn expect_str() -> Token<'a> {
        Token::String(RawString {
            prefix: None,
            val: vec![CharToken::Error],
        })
    }

    pub fn expect_unicode_str() -> Token<'a> {
        Token::String(RawString {
            prefix: Some("u"),
            val: vec![CharToken::Error],
        })
    }

    pub fn expect_regex_str() -> Token<'a> {
        Token::String(RawString {
            prefix: Some("r"),
            val: vec![CharToken::Error],
        })
    }

    pub fn expect_char() -> Token<'a> {
        Token::Char(RawChar {
            prefix: None,
            val: CharToken::Error,
        })
    }

    pub fn expect_unicode_char() -> Token<'a> {
        Token::Char(RawChar {
            prefix: Some("u"),
            val: CharToken::Error,
        })
    }

    pub fn is_ident(&self) -> bool {
        matches!(self, Token::Ident(_))
    }

    pub fn map_ident(span: Span<'a>, this: Self) -> Result<String, Error<'a>> {
        match this {
            Token::Ident(i) => Ok(i.to_string()),
            _ => Err(Error::expected_input_found(
                span,
                [Some(Token::Ident("..."))],
                Some(this),
            )),
        }
    }

    pub fn is_ident_lit(str: &'static str) -> impl Fn(&Token) -> bool + Clone {
        move |t| matches!(t, Token::Ident(s) if *s == str)
    }

    pub fn is_newline(&self) -> bool {
        *self == Token::Newline
    }
}
