use crate::tokens::Token;

use logos::Logos;
use std::ops;

pub fn tokenize(src: &str) -> impl Iterator<Item = (Token<'_>, ops::Range<usize>)> {
    Token::lexer(src)
        .spanned()
        .filter(|(t, _)| !matches!(t, Token::Comment(_)))
}

pub fn tokenize_with_comments(src: &str) -> impl Iterator<Item = (Token<'_>, ops::Range<usize>)> {
    Token::lexer(src)
        .spanned()
}
