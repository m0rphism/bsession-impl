use crate::lexer::Token;
use crate::regex::parser::regex_parser;
use crate::regex::Regex as RegexS;
use crate::syntax::Eff as EffS;
use crate::syntax::Id as IdS;
use crate::syntax::*;
use crate::util::lexer_offside::Braced;
use crate::util::peg_logos::SpannedToks;
use crate::util::span::{Span, Spanned};

use Braced::Token as Tok;

#[cfg_attr(rustfmt, rustfmt_skip)]
peg::parser! {
    pub grammar rlang_parser<'a>(toks: &'a SpannedToks<'a, Braced<Token<'a>>>) for SpannedToks<'a, Braced<Token<'a>>> {
        use Token::*;

        rule spanned<T>(t: rule<T>) -> Spanned<T>
            = start:position!() x:t() end:position!() {
                // TODO: For some reason the position reporting in `peg_logos` doesn't work,
                // so we use this workaround to convert token-spans to byte-spans ourself...
                let start = toks.toks.get(start)
                        .map(|t| t.span.start)
                        .unwrap_or_else(|| toks.toks.last().unwrap().span.end);
                let end = if end > 0 { end - 1 } else { end };
                let end = toks.toks.get(end)
                        .map(|t| t.span.end)
                        .unwrap_or_else(|| toks.toks.last().unwrap().span.end);
                Spanned::new(x, Span { start, end })
            }

        // Identifier

        pub rule id() -> IdS = quiet!{[Tok(Id(x))] { x.to_owned() }} / expected!("identifier")
        pub rule sid() -> SId = spanned(<id()>)

        pub rule tok(t: Token<'a>) -> () = quiet!{[Tok(t2) if t == t2] { () }} / expected!(t.to_str())

        // Multiplicities

        pub rule mult() -> Mult
            = tok(Unr) { Mult::Unr }
            / tok(Lin) { Mult::Lin }
            / tok(Left) { Mult::OrdL }
            / tok(Right) { Mult::OrdR }
        pub rule smult() -> SMult = spanned(<mult()>)

        // Effects

        pub rule effect() -> EffS
            = tok(Pure) { EffS::No }
            / tok(Eff) { EffS::Yes }
        pub rule seffect() -> SEff = spanned(<effect()>)

        // Types

        pub rule type_() -> Type = t:type_arrow() { t }
        pub rule stype() -> SType = spanned(<type_()>)

        #[cache_left_rec]
        pub rule type_arrow() -> Type
            = t1:stype_arrow() tok(Minus) tok(BracketL) m:smult() tok(Semicolon) e:seffect() tok(BracketR) tok(Arrow) t2:stype_prod() { Type::Arr(m, e, Box::new(t1), Box::new(t2)) }
            / t:type_prod() { t }
        pub rule stype_arrow() -> SType = spanned(<type_arrow()>)

        pub rule type_prod() -> Type
            = t1:stype_atom() tok(Star) tok(BracketL) m:smult() tok(BracketR) t2:stype_prod() { Type::Prod(m, Box::new(t1), Box::new(t2)) }
            / t:type_atom() { t }
         
        pub rule stype_prod() -> SType = spanned(<type_prod()>)

        pub rule type_atom() -> Type
            = tok(UnitT) { Type::Unit }
            / r:sregex() { Type::Regex(r) }
        pub rule stype_atom() -> SType = spanned(<type_atom()>)

        // Expressions

        pub rule expr() -> Expr = e:expr_ann() { e }
        pub rule sexpr() -> SExpr = spanned(<expr()>)

        pub rule expr_ann() -> Expr
            = e:sexpr_lam() tok(Colon) t:stype() { Expr::Ann(Box::new(e), t) }
            / e:expr_lam() { e }
        pub rule sexpr_ann() -> SExpr = spanned(<expr_ann()>)

        pub rule smult_opt() -> Option<SMult>
            = om:(tok(BracketL) m:smult() tok(BracketR) { m })? { om }

        #[cache]
        pub rule expr_lam() -> Expr
            = tok(Lambda) m:smult_opt() x:sid() tok(Period) e:sexpr_lam() { Expr::Abs(m, x, Box::new(e)) }
            / tok(Let) x:sid() tok(Comma) y:sid() tok(Equals) e1:sexpr_ann() tok(In) e2:sexpr_lam() { Expr::LetPair(x, y, Box::new(e1), Box::new(e2)) }
            / tok(Let) x:sid() tok(Equals) e1:sexpr_ann() tok(In) e2:sexpr_lam() { Expr::Let(x, Box::new(e1), Box::new(e2)) }
            / e:expr_seq() { e }
        pub rule sexpr_lam() -> SExpr = spanned(<expr_lam()>)

        #[cache_left_rec]
        pub rule expr_seq() -> Expr
            = e1:sexpr_app() tok(Semicolon) e2:sexpr_seq() { Expr::Seq(Box::new(e1), Box::new(e2)) }
            / e:expr_app() { e }
        pub rule sexpr_seq() -> SExpr = spanned(<expr_seq()>)

        #[cache_left_rec]
        pub rule expr_app() -> Expr
            = tok(New) r:sregex() { Expr::New(r) }
            / tok(Bang) w:sword() e:sexpr_atom() { Expr::Write(w, Box::new(e)) }
            / tok(Split) r:sregex() e:sexpr_atom() { Expr::Split(r, Box::new(e)) }
            / tok(Close) e:sexpr_atom() { Expr::Close(Box::new(e)) }
            / e1:sexpr_app() tok(Amp) x:sid() { Expr::AppBorrow(Box::new(e1), x) }
            / e1:sexpr_app() e2:sexpr_atom() { Expr::App(Box::new(e1), Box::new(e2)) }
            / e:expr_atom() { e }
        pub rule sexpr_app() -> SExpr = spanned(<expr_app()>)

        #[cache]
        pub rule expr_atom() -> Expr
            = tok(ParenL) e:expr() tok(ParenR) { e }
            / tok(ParenL) e1:sexpr() tok(Comma) m:smult_opt() e2:sexpr() tok(ParenR) { Expr::Pair(m, Box::new(e1), Box::new(e2)) }
            / tok(Unit) { Expr::Unit }
            / x:sid() { Expr::Var(x.to_owned()) }
        pub rule sexpr_atom() -> SExpr = spanned(<expr_atom()>)

        // Regular Expressions

        pub rule regex() -> RegexS<u8>
            = quiet!{[Tok(Regex(""))] { crate::regex::Regex::Eps }}
            / quiet!{[Tok(Regex(s))] {? regex_parser::expr_u8(s).map_err(|e| "Failed parsing regex") }}
            / expected!("regex")
        pub rule sregex() -> SRegex = spanned(<regex()>)

        // Regular Expression Words

        pub rule word() -> Word = quiet!{[Tok(Str(s))] { s.to_string() }} / expected!("string")
        pub rule sword() -> SWord = spanned(<word()>)

        // Whole Programs

        pub rule program() -> Expr = [BlockStart] [BlockItem] e:expr() [BlockEnd] { e }
        pub rule sprogram() -> SExpr = spanned(<program()>)

        // #[cache_left_rec]
        // pub rule expr() -> Expr = precedence!{
        //     // e1:@ tok(Or) e2:(@) { Expr::Binop(Binop::Or, Box::new(e1), Box::new(e2)) }
        //     // --
        //     // e1:@ tok(And) e2:(@) { Expr::Binop(Binop::And, Box::new(e1), Box::new(e2)) }
        //     // --
        //     // tok(Not) e:(@) { Expr::Unop(Unop::Not, Box::new(e)) }
        //     // --
        //     // // TODO: (in-)equality chains
        //     // e1:@ tok(Lt) e2:(@) { Expr::Binop(Binop::Lt, Box::new(e1), Box::new(e2)) }
        //     // e1:@ tok(Le) e2:(@) { Expr::Binop(Binop::Le, Box::new(e1), Box::new(e2)) }
        //     // e1:@ tok(Gt) e2:(@) { Expr::Binop(Binop::Gt, Box::new(e1), Box::new(e2)) }
        //     // e1:@ tok(Ge) e2:(@) { Expr::Binop(Binop::Ge, Box::new(e1), Box::new(e2)) }
        //     // e1:@ tok(DoubleEquals) e2:(@) { Expr::Binop(Binop::Eq, Box::new(e1), Box::new(e2)) }
        //     // e1:@ tok(BangEquals) e2:(@) { Expr::Binop(Binop::Neq, Box::new(e1), Box::new(e2)) }
        //     // --
        //     // e1:(@) tok(BracketL) e2:expr() tok(BracketR) { Expr::ListAccess(Box::new(e1), Box::new(e2)) }
        //     // e1:@ tok(Plus) e2:(@) { Expr::Binop(Binop::Add, Box::new(e1), Box::new(e2)) }
        //     // e1:(@) tok(Minus) e2:@ { Expr::Binop(Binop::Sub, Box::new(e1), Box::new(e2)) }
        //     // e1:@ tok(Star) e2:(@) { Expr::Binop(Binop::Mul, Box::new(e1), Box::new(e2)) }
        //     // e1:(@) tok(Slash) e2:@ { Expr::Binop(Binop::Div, Box::new(e1), Box::new(e2)) } // TODO: right assoc?
        //     // e1:(@) tok(DoubleSlash) e2:@ { Expr::Binop(Binop::Div, Box::new(e1), Box::new(e2)) } // TODO
        //     // --
        //     // tok(Minus) e:(@) { Expr::Unop(Unop::Neg, Box::new(e)) }
        //     // --
        //     tok(Lambda) x:id() tok(Colon) e:(@) { Expr::Abs(x, Box::new(e)) }
        //     --
        //     e1:(@) e2:@ { Expr::App(Box::new(e1), Box::new(e2)) }
        //     --
        //     // tok(BracketL) es:args() tok(BracketR)  { Expr::List(es) }
        //     // tok(None) { Expr::None }
        //     // [Tok(Int(x))] { Expr::Int(x) }
        //     // [Tok(Float(x))] { Expr::Float(x) }
        //     // tok(True) { Expr::Bool(true) }
        //     // tok(False) { Expr::Bool(false) }
        //     // [Tok(Str(x))] { Expr::String(x.to_owned()) } // TODO: remove quotes
        //     [Tok(Id(x))] { Expr::Var(x.to_owned()) }
        // }
    }
}

// #[test]
// fn parse_int() {
//     assert_eq!(nix_parser::expr("-323"), Ok(Expr::Int(-323)));
//     assert_eq!(
//         nix_parser::expr_s("-323"),
//         Ok(Spanned::new(Expr::Int(-323), Span { start: 0, end: 4 }))
//     );
// }
