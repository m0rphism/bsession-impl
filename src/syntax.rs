use crate::{regex, util::span::Spanned};
use std::hash::Hash;

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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Mult {
    Unr,
    Lin,
    OrdL,
    OrdR,
}
pub type SMult = Spanned<Mult>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Eff {
    Yes,
    No,
}
pub type SEff = Spanned<Eff>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    Unit,
    New(SRegex),
    Write(SWord, Box<SExpr>),
    Split(SRegex, Box<SExpr>),
    Close(Box<SExpr>),
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

impl Type {
    pub fn is_subtype_of(&self, other: &Type) -> bool {
        match (self, other) {
            (Type::Unit, Type::Unit) => true,
            (Type::Regex(r1), Type::Regex(r2)) => r1.is_subseteq_of(r2),
            (Type::Arr(m1, p1, t11, t12), Type::Arr(m2, p2, t21, t22)) => {
                m1 == m2 && p1 == p2 && t11.is_subtype_of(t21) && t22.is_subtype_of(t12)
            }
            (Type::Prod(m1, t11, t12), Type::Prod(m2, t21, t22)) => {
                m1 == m2 && t11.is_equal_to(t21) && t12.is_equal_to(t22)
            }
            (_, _) => false,
        }
    }
    pub fn is_equal_to(&self, other: &Type) -> bool {
        match (self, other) {
            (Type::Unit, Type::Unit) => true,
            (Type::Regex(r1), Type::Regex(r2)) => r1.is_equal_to(r2),
            (Type::Arr(m1, p1, t11, t12), Type::Arr(m2, p2, t21, t22)) => {
                m1 == m2 && p1 == p2 && t11.is_equal_to(t21) && t12.is_equal_to(t22)
            }
            (Type::Prod(m1, t11, t12), Type::Prod(m2, t21, t22)) => {
                m1 == m2 && t11.is_equal_to(t21) && t12.is_equal_to(t22)
            }
            (_, _) => false,
        }
    }

    pub fn is_unr(&self) -> bool {
        match self {
            Type::Unit => true,
            Type::Regex(_) => false,
            Type::Arr(m, _, _, _) => m.val == Mult::Unr,
            Type::Prod(m, _, _) => m.val == Mult::Unr,
        }
    }

    pub fn is_ord(&self) -> bool {
        !self.is_unr()
    }
}

impl Eff {
    pub fn lub(p1: Eff, p2: Eff) -> Eff {
        match p1 {
            Eff::Yes => Eff::Yes,
            Eff::No => p2,
        }
    }

    pub fn leq(e1: Eff, e2: Eff) -> bool {
        match (e1, e2) {
            (Eff::Yes, Eff::Yes) => true,
            (Eff::Yes, Eff::No) => false,
            (Eff::No, _) => true,
        }
    }
}
