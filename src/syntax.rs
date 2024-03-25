use crate::{regex, span::Spanned};

pub type Id = String;
pub type SId = Spanned<Id>;

pub type Loc = usize;
pub type SLoc = Spanned<Loc>;

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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Mult {
    Unr,
    Lin,
    OrdL,
    OrdR,
}
pub type SMult = Spanned<Mult>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Eff {
    Yes,
    No,
}
pub type SEff = Spanned<Eff>;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Unit,
    Regex(SRegex),
    Arr(SMult, SEff, Box<SType>, Box<SType>),
    Prod(SMult, Box<SType>, Box<SType>),
}
pub type SType = Spanned<Type>;

pub type Regex = regex::Regex<u8>;
pub type SRegex = Spanned<Regex>;

pub type Word = String;
pub type SWord = Spanned<Word>;

#[derive(Debug, Clone, PartialEq)]
pub enum Const {
    Unit,
    New(SRegex),
    Write(SWord),
    Split(SRegex),
    Close,
}
pub type SConst = Spanned<Const>;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Const(SConst),
    Loc(SLoc),
    Var(SId),
    Abs(SMult, SId, Box<SExpr>),
    App(SMult, Box<SExpr>, Box<SExpr>),
    Pair(SMult, Box<SExpr>, Box<SExpr>),
    Let(SMult, SId, SId, Box<SExpr>, Box<SExpr>),
    Ann(Box<SExpr>, SType),
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
pub type SExpr = Spanned<Expr>;
