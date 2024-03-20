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

use crate::pretty::{pretty, Pretty, PrettyOpts};

fn main() {
    let args = Args::parse();

    println!("===== SRC =====");
    let src = std::fs::read_to_string(&args.src_path).unwrap();
    println!("{src}\n");

    println!("===== TOKS =====");
    let toks = lexer::lex(&src).unwrap();
    for t in &toks.toks {
        println!("{t:?}");
    }
    println!();

    println!("===== TOKS OFFSIDE =====");
    let toks = lexer_offside::process_indent(toks, |_| false, |_| false);
    for t in &toks.toks {
        println!("{t:?}");
    }
    println!();

    println!("===== AST =====");
    let e = parser::py_parser::program(&toks).unwrap();
    println!("{e:?}");
    println!();

    println!("===== PRETTY =====");
    let p_opts = PrettyOpts {
        indent_by: 2,
        max_line_len: 80,
    };
    println!("{}", pretty(&p_opts, e));

    let mut colors = ColorGenerator::new();

    // Generate & choose some colours for each of our elements
    let a = colors.next();
    let b = colors.next();
    let out = Color::Fixed(81);

    let src_path = args.src_path.to_string_lossy();

    Report::build(ReportKind::Error, &src_path, 12)
        .with_config(ariadne::Config::default().with_index_type(IndexType::Byte))
        .with_code(3)
        .with_message(format!("Incompatible types"))
        .with_label(
            Label::new((&src_path, 2..3))
                .with_message(format!("This is of type {}", "Nat".fg(a)))
                .with_color(a),
        )
        .with_label(
            Label::new((&src_path, 12..13))
                .with_message(format!("This is of type {}", "Str".fg(b)))
                .with_color(b),
        )
        .with_note(format!(
            "Outputs of {} expressions must coerce to the same type",
            "match".fg(out)
        ))
        .finish()
        .print((&src_path, Source::from(&src)))
        .unwrap();

    // if let Err(e) = tui::run(&args) {
    //     println!("ERROR: {}", e);
    // }
}
