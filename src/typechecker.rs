use crate::syntax::{Eff, Expr, Id, Type};

#[derive(Debug, Clone)]
pub enum TypeError {
    //
}

#[derive(Debug, Clone)]
pub enum JoinOrd {
    Ordered,
    Unordered,
}

#[derive(Debug, Clone)]
pub enum Ctx {
    Empty,
    Bind(Id, Type),
    Join(Box<Ctx>, Box<Ctx>, JoinOrd),
}

pub type Trace = Vec<(Id, Type)>;

pub fn infer_type_(ctx: &Ctx, e: &Expr) -> Result<(Type, Eff, Trace), TypeError> {
    Ok((Type::Unit, Eff::No, vec![]))
}

pub fn infer_type(e: &Expr) -> Result<(Type, Eff), TypeError> {
    let (t, e, T) = infer_type_(&Ctx::Empty, e)?;
    Ok((t, e))
}
