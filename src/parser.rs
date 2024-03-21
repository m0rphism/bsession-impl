use crate::lexer::Token;
use crate::lexer_offside::Braced;
use crate::peg_logos::SpannedToks;
use crate::span::{Span, Spanned};
use crate::syntax::Id as IdS;
use crate::syntax::*;

use Braced::Token as Tok;
use Braced::{Begin, End, Item};

#[cfg_attr(rustfmt, rustfmt_skip)]
peg::parser! {
    pub grammar py_parser<'a>() for SpannedToks<'a, Braced<Token<'a>>> {
        use Token::*;
        rule spanned<T>(t: rule<T>) -> Spanned<T>
            = start:position!() x:t() end:position!() { Spanned::new(x, Span { start, end }) }

        pub rule id() -> IdS = [Tok(Id(x))] { x.to_owned() }
        pub rule sid() -> SId = spanned(<id()>)

        pub rule expr() -> Expr = e:expr_lam() { e }
        pub rule sexpr() -> SExpr = spanned(<expr()>)

        pub rule expr_lam() -> Expr
            = [Tok(Lambda)] x:sid() [Tok(Period)] e:sexpr_lam() { Expr::Abs(x, Box::new(e)) }
            / e:expr_app() { e }
        pub rule sexpr_lam() -> SExpr = spanned(<expr_lam()>)

        #[cache_left_rec]
        pub rule expr_app() -> Expr
            = e1:sexpr_app() e2:sexpr_atom() { Expr::App(Box::new(e1), Box::new(e2)) }
            / e:expr_atom() { e }
        pub rule sexpr_app() -> SExpr = spanned(<expr_app()>)

        pub rule expr_atom() -> Expr
            = x:sid() { Expr::Var(x.to_owned()) }
            / [Tok(ParenL)] e:expr() [Tok(ParenR)] { e }
        pub rule sexpr_atom() -> SExpr = spanned(<expr_atom()>)

        pub rule program() -> Expr = [BlockStart] [BlockItem] e:expr() [BlockEnd] { e }
        pub rule sprogram() -> SExpr = spanned(<program()>)

        // #[cache_left_rec]
        // pub rule expr() -> Expr = precedence!{
        //     // e1:@ [Tok(Or)] e2:(@) { Expr::Binop(Binop::Or, Box::new(e1), Box::new(e2)) }
        //     // --
        //     // e1:@ [Tok(And)] e2:(@) { Expr::Binop(Binop::And, Box::new(e1), Box::new(e2)) }
        //     // --
        //     // [Tok(Not)] e:(@) { Expr::Unop(Unop::Not, Box::new(e)) }
        //     // --
        //     // // TODO: (in-)equality chains
        //     // e1:@ [Tok(Lt)] e2:(@) { Expr::Binop(Binop::Lt, Box::new(e1), Box::new(e2)) }
        //     // e1:@ [Tok(Le)] e2:(@) { Expr::Binop(Binop::Le, Box::new(e1), Box::new(e2)) }
        //     // e1:@ [Tok(Gt)] e2:(@) { Expr::Binop(Binop::Gt, Box::new(e1), Box::new(e2)) }
        //     // e1:@ [Tok(Ge)] e2:(@) { Expr::Binop(Binop::Ge, Box::new(e1), Box::new(e2)) }
        //     // e1:@ [Tok(DoubleEquals)] e2:(@) { Expr::Binop(Binop::Eq, Box::new(e1), Box::new(e2)) }
        //     // e1:@ [Tok(BangEquals)] e2:(@) { Expr::Binop(Binop::Neq, Box::new(e1), Box::new(e2)) }
        //     // --
        //     // e1:(@) [Tok(BracketL)] e2:expr() [Tok(BracketR)] { Expr::ListAccess(Box::new(e1), Box::new(e2)) }
        //     // e1:@ [Tok(Plus)] e2:(@) { Expr::Binop(Binop::Add, Box::new(e1), Box::new(e2)) }
        //     // e1:(@) [Tok(Minus)] e2:@ { Expr::Binop(Binop::Sub, Box::new(e1), Box::new(e2)) }
        //     // e1:@ [Tok(Star)] e2:(@) { Expr::Binop(Binop::Mul, Box::new(e1), Box::new(e2)) }
        //     // e1:(@) [Tok(Slash)] e2:@ { Expr::Binop(Binop::Div, Box::new(e1), Box::new(e2)) } // TODO: right assoc?
        //     // e1:(@) [Tok(DoubleSlash)] e2:@ { Expr::Binop(Binop::Div, Box::new(e1), Box::new(e2)) } // TODO
        //     // --
        //     // [Tok(Minus)] e:(@) { Expr::Unop(Unop::Neg, Box::new(e)) }
        //     // --
        //     [Tok(Lambda)] x:id() [Tok(Colon)] e:(@) { Expr::Abs(x, Box::new(e)) }
        //     --
        //     e1:(@) e2:@ { Expr::App(Box::new(e1), Box::new(e2)) }
        //     --
        //     // [Tok(BracketL)] es:args() [Tok(BracketR)]  { Expr::List(es) }
        //     // [Tok(None)] { Expr::None }
        //     // [Tok(Int(x))] { Expr::Int(x) }
        //     // [Tok(Float(x))] { Expr::Float(x) }
        //     // [Tok(True)] { Expr::Bool(true) }
        //     // [Tok(False)] { Expr::Bool(false) }
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
