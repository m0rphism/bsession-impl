use crate::span::Spanned;

pub type Id = String;
pub type Loc = usize;

// #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
// pub enum Binop {
//     Add,
//     Mul,
//     Sub,
//     Div,
//     And,
//     Or,
//     Lt,
//     Le,
//     Gt,
//     Ge,
//     Eq,
//     Neq,
// }

// #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
// pub enum Unop {
//     Not,
//     Neg,
// }

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Var(SId),
    Abs(SId, Box<SExpr>),
    App(Box<SExpr>, Box<SExpr>),
    // Int(i64),
    // Float(f64),
    // String(String),
    // Bool(bool),
    // List(Vec<Expr>),
    // None,
    // ListAccess(Box<Expr>, Box<Expr>),
    // Binop(Binop, Box<Expr>, Box<Expr>),
    // Unop(Unop, Box<Expr>),
    // Scope(Program),
    // Loc(Loc),
}

pub type SId = Spanned<Id>;
pub type SExpr = Spanned<Expr>;
