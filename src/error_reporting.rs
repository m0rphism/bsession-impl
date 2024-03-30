use std::ops::Range;

use crate::{
    lexer::LexerError,
    pretty,
    type_checker::TypeError,
    util::pretty::{pretty_def, PrettyOpts},
};
use ariadne::{Color, ColorGenerator, Fmt, IndexType, Label, Report, ReportKind, Source};
use peg::error::ParseError;

#[derive(Debug, Clone)]
pub enum IErr {
    Lexer(LexerError),
    Parser(ParseError<usize>),
    Typing(TypeError),
}

pub struct CSource {
    pub path: String,
    pub data: String,
}

pub struct CLabel {
    pub span: Range<usize>,
    pub msg: String,
}

pub fn label(span: Range<usize>, msg: impl AsRef<str>) -> CLabel {
    CLabel {
        span,
        msg: msg.as_ref().to_string(),
    }
}

fn report(
    src: &CSource,
    loc: usize,
    msg: impl AsRef<str>,
    labels: impl IntoIterator<Item = CLabel>,
) {
    let mut colors = ColorGenerator::new();
    let a = colors.next();
    Report::build(ReportKind::Error, &src.path, loc)
        .with_config(ariadne::Config::default().with_index_type(IndexType::Byte))
        .with_message(msg.as_ref())
        .with_labels(labels.into_iter().map(|l| {
            Label::new((&src.path, l.span))
                .with_message(l.msg)
                .with_color(a)
        }))
        .finish()
        .print((&src.path, Source::from(&src.data)))
        .unwrap();
}

pub fn report_error(src_path: &str, src: &str, e: IErr) {
    let src = CSource {
        path: src_path.to_string(),
        data: src.to_string(),
    };
    let mut colors = ColorGenerator::new();
    let a = colors.next();

    let p_opts = PrettyOpts {
        indent_by: 2,
        max_line_len: 80,
    };

    match e {
        IErr::Lexer(e) => {
            report(
                &src,
                e.span.start,
                "Lexing failed",
                [label(e.span, "Lexer got stuck here")],
            );
        }
        IErr::Parser(e) => {
            report(
                &src,
                e.location,
                "Parsing failed",
                [label(
                    e.location..e.location,
                    format!("Expected {}", e.expected),
                )],
            );
        }
        IErr::Typing(e) => match e {
            TypeError::LocationExpr(l) => {
                report(
                    &src,
                    l.span.start,
                    "Type Error",
                    [label(
                        l.span,
                        "Location expressions are not allowed in surface syntax",
                    )],
                );
            }
            TypeError::UndefinedVariable(x) => {
                report(
                    &src,
                    x.span.start,
                    "Type Error",
                    [label(x.span, "Undefined Variable")],
                );
            }
            TypeError::Mismatch(e, t_expected, t_actual) => {
                report(
                    &src,
                    e.span.start,
                    "Type Error",
                    [
                        label(e.span, "This expression"),
                        label(t_actual.span, "has this type"),
                        label(t_expected.span, "but should have this type"),
                    ],
                );
            }
            TypeError::MismatchMult(e, m_expected, m_actual) => {
                report(
                    &src,
                    e.span.start,
                    "Type Error",
                    [
                        label(e.span, "In this expression"),
                        label(m_actual.span, "got this multiplicity"),
                        label(m_expected.span, "but should be this multiplicity"),
                    ],
                );
            }
            TypeError::MismatchEff(e, p_expected, p_actual) => {
                report(
                    &src,
                    e.span.start,
                    "Type Error",
                    [
                        label(e.span, "In this expression"),
                        label(p_actual.span, "got this effect"),
                        label(p_expected.span, "but should be this effect"),
                    ],
                );
            }
            TypeError::TypeAnnotationMissing(e) => {
                report(
                    &src,
                    e.span.start,
                    "Type Error",
                    [label(e.span, "Missing type annotation")],
                );
            }
            TypeError::ClosedUnfinished(e, r) => {
                report(
                    &src,
                    e.span.start,
                    "Type Error",
                    [
                        label(
                            e.span,
                            "Tried to close this expression of unfinished resource type",
                        ),
                        label(r.span, "Should be finished"),
                    ],
                );
            }
            TypeError::InvalidWrite(e, r, w) => {
                report(
                    &src,
                    e.span.start,
                    "Type Error",
                    [
                        label(w.span, "Invalid write of this word"),
                        label(r.span, "on this resource"),
                    ],
                );
            }
            TypeError::InvalidSplitArg(r1) => {
                report(
                    &src,
                    r1.span.start,
                    "Type Error",
                    [label(
                        r1.span,
                        "Splitting by an empty prefix is not allowed.",
                    )],
                );
            }
            TypeError::InvalidSplitRes(r, r1, r2) => {
                report(
                    &src,
                    r1.span.start,
                    "Type Error",
                    [
                        label(r.span, "Splitting this regex"),
                        label(r1.span, "by this prefix"),
                        label(r2.span, "results in this empty suffix"),
                    ],
                );
            }
            TypeError::CtxSplitFailed(e, ctx, ctx2) => {
                report(
                    &src,
                    e.span.start,
                    "Type Error",
                    [
                        label(
                            e.span,
                            format!(
                                "In this expression, splitting the context {} resulted in the invalid context {}",
                                pretty_def(&ctx),
                                pretty_def(&ctx2)
                            )
                        ),
                    ],
                );
            }
            TypeError::CtxCtxSplitFailed(e, ctx, xs) => {
                report(
                    &src,
                    e.span.start,
                    "Type Error",
                    [label(
                        e.span,
                        format!(
                            "In this expression, failed to split context {} by free variables {}",
                            pretty_def(&ctx),
                            pretty_def(&xs)
                        ),
                    )],
                );
            }
            TypeError::Shadowing(e, x) => {
                report(
                    &src,
                    e.span.start,
                    "Type Error",
                    [label(
                        e.span,
                        format!(
                            "In this expression, the variable {} is introduced, which shadows another variable.",
                            pretty_def(&x)
                        ),
                    )],
                );
            }
            TypeError::CtxNotUnr(e, ctx) => {
                report(
                    &src,
                    e.span.start,
                    "Type Error",
                    [label(
                        e.span,
                        format!(
                            "This unrestricted lambda abstraction tries to capture a non-unrestricted context {}",
                            pretty_def(&ctx)
                        ),
                    )],
                );
            }
            TypeError::NewEmpty(r) => {
                report(
                    &src,
                    r.span.start,
                    "Type Error",
                    [label(
                        r.span,
                        "This resource is unsatisfiable (empty language).",
                    )],
                );
            }
            _ => println!("{e:?}"),
        },
    }
}
