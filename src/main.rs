pub mod args;
pub mod lexer;
pub mod lexer_offside;
pub mod parser;
pub mod peg_logos;
pub mod pretty;
pub mod span;
pub mod syntax;
pub mod to_str;

use args::Args;
use ariadne::{Color, ColorGenerator, Fmt, IndexType, Label, Report, ReportKind, Source};
use clap::Parser;
use lexer::LexerError;
use peg::error::ParseError;

use crate::pretty::{pretty, PrettyOpts};

pub enum IErr {
    Lexer(LexerError),
    Parser(ParseError<usize>),
}

pub fn run(args: &Args) -> Result<(), IErr> {
    println!("===== SRC =====");
    let src = std::fs::read_to_string(&args.src_path).unwrap();
    println!("{src}\n");

    // println!("===== TOKS =====");
    let toks = lexer::lex(&src).map_err(IErr::Lexer)?;
    // for (i, t) in toks.toks.iter().enumerate() {
    //     println!("{i}:\t{t:?}");
    // }
    // println!();

    println!("===== TOKS OFFSIDE =====");
    let toks = lexer_offside::process_indent(toks, |_| false, |_| false);
    for (i, t) in toks.toks.iter().enumerate() {
        println!("{i}:\t{t:?}");
    }
    println!();

    println!("===== AST =====");
    let e = parser::py_parser::program(&toks).map_err(IErr::Parser)?;
    println!("{e:?}");
    println!();

    println!("===== PRETTY =====");
    let p_opts = PrettyOpts {
        indent_by: 2,
        max_line_len: 80,
    };
    println!("{}", pretty(&p_opts, e));

    Ok(())
}

pub fn report_error(src_path: &str, e: IErr) {
    let src = std::fs::read_to_string(src_path).unwrap();

    let mut colors = ColorGenerator::new();
    let a = colors.next();

    match e {
        IErr::Lexer(e) => {
            Report::build(ReportKind::Error, &src_path, e.span.start)
                .with_config(ariadne::Config::default().with_index_type(IndexType::Byte))
                .with_message(format!("Lexing failed"))
                .with_label(
                    Label::new((&src_path, e.span))
                        .with_message(format!("Lexer got stuck here"))
                        .with_color(a),
                )
                .finish()
                .print((&src_path, Source::from(&src)))
                .unwrap();
        }
        IErr::Parser(e) => {
            Report::build(ReportKind::Error, &src_path, e.location) // TODO: here IndexType::Byte doesn't apply...
                .with_config(ariadne::Config::default().with_index_type(IndexType::Byte))
                .with_message(format!("Parsing failed"))
                .with_label(
                    Label::new((&src_path, e.location..e.location))
                        .with_message(format!("Expected {}", e.expected))
                        .with_color(a),
                )
                .finish()
                .print((&src_path, Source::from(&src)))
                .unwrap();
        }
    }
}

fn main() {
    let args = Args::parse();
    let src_path = args.src_path.to_string_lossy();
    if let Err(e) = run(&args) {
        report_error(&src_path, e);
    }
}
