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
    // if let Err(e) = tui::run(&args) {
    //     println!("ERROR: {}", e);
    // }
}
