pub mod args;
pub mod lexer;
pub mod parser;
pub mod regex;
pub mod syntax;
pub mod pretty;
pub mod typechecker;
pub mod util;

use std::ops::Range;

use args::Args;
use ariadne::{Color, ColorGenerator, Fmt, IndexType, Label, Report, ReportKind, Source};
use clap::Parser;
use lexer::LexerError;
use peg::error::ParseError;
use typechecker::TypeError;

use crate::{
    lexer::Token,
    util::lexer_offside::{self, Braced},
    util::pretty::{pretty, PrettyOpts},
};

pub enum IErr {
    Lexer(LexerError),
    Parser(ParseError<usize>),
    Typing(TypeError),
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
    let e = parser::py_parser::sprogram(&toks).map_err(IErr::Parser)?;
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
    println!("Type: {}", pretty(&p_opts, t));
    println!("Effect: {}", pretty(&p_opts, e));

    Ok(())
}

pub fn report_error(src_path: &str, e: IErr) {
    let src = std::fs::read_to_string(src_path).unwrap();

    let mut colors = ColorGenerator::new();
    let a = colors.next();

    let p_opts = PrettyOpts {
        indent_by: 2,
        max_line_len: 80,
    };

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
        IErr::Typing(e) => {
            match e {
                TypeError::LocationExpr(l) => {
                    Report::build(
                    ReportKind::Error,
                    &src_path,
                    l.span.start,
                ) // TODO: here IndexType::Byte doesn't apply...
                .with_config(ariadne::Config::default().with_index_type(IndexType::Byte))
                .with_message(format!("Type Error"))
                .with_label(
                    Label::new((&src_path, l.span))
                        .with_message(format!(
                            "Location expressions are not allowed in surface syntax"
                        ))
                        .with_color(a),
                )
                .finish()
                .print((&src_path, Source::from(&src)))
                .unwrap()
                }
                TypeError::UndefinedVariable(x) => {
                    Report::build(
                    ReportKind::Error,
                    &src_path,
                    x.span.start,
                ) // TODO: here IndexType::Byte doesn't apply...
                .with_config(ariadne::Config::default().with_index_type(IndexType::Byte))
                .with_message(format!("Type Error"))
                .with_label(
                    Label::new((&src_path, x.span))
                        .with_message(format!(
                            "Undefined variable"
                        ))
                        .with_color(a),
                )
                .finish()
                .print((&src_path, Source::from(&src)))
                .unwrap()
                }
                TypeError::LeftOver(c) => {
                    println!("Type Error: Unused ordered variables left: {c:?}");
                }
                TypeError::Mismatch(t_expected, t_actual) => {
                    Report::build(ReportKind::Error, &src_path, t_expected.span.start)
                        .with_config(ariadne::Config::default().with_index_type(IndexType::Byte))
                        .with_message(format!("Type Error"))
                        .with_label(
                            Label::new((&src_path, t_expected.span))
                                .with_message(format!(
                                    "Expected type '{}'",
                                    pretty(&p_opts, t_expected.val)
                                ))
                                .with_color(a),
                        )
                        .with_label(
                            Label::new((&src_path, t_actual.span))
                                .with_message(format!(
                                    "Actual type '{}'",
                                    pretty(&p_opts, t_actual.val)
                                ))
                                .with_color(a),
                        )
                        .finish()
                        .print((&src_path, Source::from(&src)))
                        .unwrap();
                }
                TypeError::MismatchMult(m_expected, m_actual) => {
                    Report::build(ReportKind::Error, &src_path, m_expected.span.start)
                        .with_config(ariadne::Config::default().with_index_type(IndexType::Byte))
                        .with_message(format!("Type Error"))
                        .with_label(
                            Label::new((&src_path, m_expected.span))
                                .with_message(format!(
                                    "Expected multiplicity '{}'",
                                    pretty(&p_opts, m_expected.val)
                                ))
                                .with_color(a),
                        )
                        .with_label(
                            Label::new((&src_path, m_actual.span))
                                .with_message(format!(
                                    "Actual multiplicity '{}'",
                                    pretty(&p_opts, m_actual.val),
                                ))
                                .with_color(a),
                        )
                        .finish()
                        .print((&src_path, Source::from(&src)))
                        .unwrap();
                }
                TypeError::TypeAnnotationMissing(e) => {
                    Report::build(ReportKind::Error, &src_path, e.span.start)
                        .with_config(ariadne::Config::default().with_index_type(IndexType::Byte))
                        .with_message(format!("Type Error"))
                        .with_label(
                            Label::new((&src_path, e.span))
                                .with_message(format!("Missing type annotation."))
                                .with_color(a),
                        )
                        .finish()
                        .print((&src_path, Source::from(&src)))
                        .unwrap();
                }
                _ => println!("{e:?}"),
            }
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
