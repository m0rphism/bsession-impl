use crate::span::Spanned;

pub type Id = String;
pub type SId = Spanned<Id>;

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
pub enum Mult {
    Unr,
    Lin,
    OrdL,
    OrdR,
}
pub type SMult = Spanned<Mult>;

#[derive(Debug, Clone, PartialEq)]
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

pub type Regex = ();
pub type SRegex = Spanned<Regex>;

pub type Word = ();
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
    Loc(Loc),
    Var(SId),
    Abs(SId, Box<SExpr>),
    App(Box<SExpr>, Box<SExpr>),
    Pair(Box<SExpr>, Box<SExpr>),
    Let(SId, SId, Box<SExpr>, Box<SExpr>),
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
