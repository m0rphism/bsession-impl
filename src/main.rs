pub mod args;
pub mod error_reporting;
pub mod lexer;
pub mod parser;
pub mod pretty;
pub mod regex;
pub mod syntax;
pub mod tests;
pub mod typechecker;
pub mod util;

use std::process::exit;

use clap::Parser;

use crate::{
    args::Args,
    error_reporting::{report_error, IErr},
    lexer::Token,
    syntax::{Eff, Type},
    util::lexer_offside::{self, Braced},
    util::pretty::{pretty, PrettyOpts},
};

fn main() {
    let args = Args::parse();
    let src_path = args.src_path.to_string_lossy();
    if let Err(e) = run(&args) {
        report_error(&src_path, e);
        exit(1)
    }
}

pub fn run(args: &Args) -> Result<(), IErr> {
    println!("===== SRC =====");
    let src = std::fs::read_to_string(&args.src_path).unwrap();
    println!("{src}\n");
    let _ = typecheck(&src);
    Ok(())
}

pub fn typecheck(src: &str) -> Result<(Type, Eff), IErr> {
    // println!("===== TOKS =====");
    let toks = lexer::lex(&src).map_err(IErr::Lexer)?;
    // for (i, t) in toks.toks.iter().enumerate() {
    //     println!("{i}:\t{t:?}");
    // }
    // println!();

    println!("===== TOKS OFFSIDE =====");
    let mut toks = lexer_offside::process_indent(toks, |_| false, |_| false);
    toks.toks = toks
        .toks
        .into_iter()
        .filter(|t| t.val != Braced::Token(Token::NewLine))
        .collect::<Vec<_>>();
    for (i, t) in toks.toks.iter().enumerate() {
        println!("{i}:\t{t:?}");
    }
    println!();

    println!("===== AST =====");
    let e = parser::rlang_parser::sprogram(&toks).map_err(IErr::Parser)?;
    println!("{e:?}");
    println!();

    println!("===== PRETTY =====");
    let p_opts = PrettyOpts {
        indent_by: 2,
        max_line_len: 80,
    };
    println!("{}", pretty(&p_opts, &e));
    println!();

    println!("===== TYPECHECKER =====");
    let (t, e) = typechecker::infer_type(&e).map_err(IErr::Typing)?;
    println!("Type: {}", pretty(&p_opts, &t));
    println!("Effect: {}", pretty(&p_opts, &e));

    Ok((t.val, e))
}
