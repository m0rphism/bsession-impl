use std::{error::Error, fmt::Display};

use logos::{Lexer, Logos};

use crate::{
    peg_logos::SpannedToks,
    span::{Span, Spanned},
};

#[derive(Debug, PartialEq, Clone, Default)]
pub enum LexingError {
    Int,
    Float,
    #[default]
    Other,
}

// TODO
impl Display for LexingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{:?}", self)
    }
}

impl Error for LexingError {}

#[derive(Logos, Clone, Copy, Debug, PartialEq)]
#[logos(skip r"[ \t\f]+")] // Ignore this regex pattern between tokens
#[logos(skip r"#[^\n]+")] // Ignore this regex pattern between tokens
#[logos(error = LexingError)]
pub enum Token<'a> {
    // Keywords
    #[token("true")]
    True,
    #[token("false")]
    False,
    #[token("let")]
    Let,
    #[token("in")]
    In,
    #[token("if")]
    If,
    #[token("then")]
    Then,
    #[token("else")]
    Else,
    #[token("or")]
    Or,
    #[token("and")]
    And,
    #[token("import")]
    Import,
    #[token("not")]
    Not,

    // Operators
    #[token(";")]
    Semicolon,
    #[token("{")]
    BraceL,
    #[token("}")]
    BraceR,
    #[token("(")]
    ParenL,
    #[token(")")]
    ParenR,
    #[token("[")]
    BracketL,
    #[token("]")]
    BracketR,
    #[token("+")]
    Plus,
    #[token("->")]
    Arrow,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("//")]
    DoubleSlash,
    #[token("/")]
    Slash,
    #[token("<=")]
    Le,
    #[token(">=")]
    Ge,
    #[token("<")]
    Lt,
    #[token(">")]
    Gt,
    #[token("==")]
    DoubleEquals,
    #[token("!=")]
    BangEquals,
    #[token("=")]
    Equals,
    #[token(",")]
    Comma,
    #[token("@")]
    At,
    #[token(":")]
    Colon,
    #[token(".")]
    Period,
    #[regex(r"\\|λ")]
    Lambda,

    // Positive Int
    #[regex(r"[0-9]+", |lex| lex.slice().parse().map_err(|_| LexingError::Int))]
    Int(i64),

    // Positive Float
    #[regex(r"([0-9]+\.[0-9]*(e[0-9]*)?)|([0-9]*\.[0-9]+(e[0-9]*)?)", |lex| lex.slice().parse().map_err(|_| LexingError::Float))]
    Float(f64),

    // String
    #[regex(r#""(\\"|[^"])*""#, |lex| &lex.slice()[1..lex.slice().len()-1])]
    #[regex(r#""(\\'|[^'])*'"#, |lex| &lex.slice()[1..lex.slice().len()-1])]
    #[regex(r"'''(\\'|[^']|'[^']|''[^'])*'''", |lex| &lex.slice()[2..lex.slice().len()-2])]
    Str(&'a str),

    // Identifier
    #[regex("[a-zA-Z_]+[a-zA-Z0-9_]*")]
    Id(&'a str),

    // Newline, later processed by module `lexer_offside`.
    #[regex("\n|\r\n|\n\r")]
    NewLine,
}

pub fn lex_plain(s: &str) -> impl Iterator<Item = (Result<Token, LexingError>, Span)> + '_ {
    let lex: Lexer<Token> = Token::lexer(s);
    lex.spanned()
}

pub fn lex(src: &str) -> Result<SpannedToks<Token>, LexingError> {
    let lex: Lexer<Token> = Token::lexer(src);
    Ok(SpannedToks {
        src,
        toks: lex
            .spanned()
            .map(|(tok, span)| tok.map(|tok| Spanned::new(tok, span)))
            .collect::<Result<Vec<_>, _>>()?,
    })
}
