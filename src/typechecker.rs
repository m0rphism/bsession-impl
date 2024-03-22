use crate::{
    span::Spanned,
    syntax::{Const, Eff, Expr, Id, Mult, SExpr, SId, SLoc, SType, Type},
};

pub fn lub(p1: Eff, p2: Eff) -> Eff {
    match p1 {
        Eff::Yes => Eff::Yes,
        Eff::No => p2,
    }
}

#[derive(Debug, Clone)]
pub enum TypeError {
    LocationExpr(SLoc),
    UndefinedVariable(SId),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum JoinOrd {
    Ordered,
    Unordered,
}

pub fn is_unr(t: &Type) -> bool {
    match t {
        Type::Unit => true,
        Type::Regex(_) => false,
        Type::Arr(m, _, _, _) => m.val == Mult::Unr,
        Type::Prod(m, _, _) => m.val == Mult::Unr,
    }
}

pub fn is_ord(t: &Type) -> bool {
    !is_unr(t)
}

#[derive(Debug, Clone)]
pub enum Ctx {
    Empty,
    Bind(Id, Type),
    Join(Box<Ctx>, Box<Ctx>, JoinOrd),
}

impl Ctx {
    pub fn map_binds(&self, f: &mut impl FnMut(&Id, &Type)) {
        match self {
            Ctx::Empty => (),
            Ctx::Bind(x, t) => f(x, t),
            Ctx::Join(c1, c2, _o) => {
                c1.map_binds(f);
                c2.map_binds(f);
            }
        }
    }
    pub fn is_unr(&self) -> bool {
        let mut unr = true;
        self.map_binds(&mut |_x, t| unr = unr && is_unr(t));
        unr
    }
    pub fn lookup_ord_pure(&self, x: &Id) -> Option<(Ctx, Type)> {
        let mut c = self.clone();
        c.lookup_ord(x).map(|t| (c, t))
    }
    pub fn lookup_ord(&mut self, x: &Id) -> Option<Type> {
        match self {
            Ctx::Empty => None,
            Ctx::Bind(y, t) if x == y => {
                if is_ord(t) {
                    let t = t.clone();
                    *self = Ctx::Empty;
                    Some(t)
                } else {
                    Some(t.clone())
                }
            }
            Ctx::Bind(_y, _t) => None,
            Ctx::Join(c1, c2, o) => c1.lookup_ord(x).or_else(|| {
                if c1.is_unr() || *o == JoinOrd::Ordered {
                    c2.lookup_ord(x)
                } else {
                    None
                }
            }),
        }
    }
}

#[derive(Debug, Clone)]
pub enum CtxCtx {
    Hole,
    JoinL(Box<CtxCtx>, Box<Ctx>, JoinOrd),
    JoinR(Box<Ctx>, Box<CtxCtx>, JoinOrd),
}

impl CtxCtx {
    pub fn fill(&self, c: Ctx) -> Ctx {
        match self {
            CtxCtx::Hole => c,
            CtxCtx::JoinL(cc1, c2, o) => Ctx::Join(Box::new(cc1.fill(c)), c2.clone(), o.clone()),
            CtxCtx::JoinR(c1, cc2, o) => Ctx::Join(c1.clone(), Box::new(cc2.fill(c)), o.clone()),
        }
    }
}

pub type Trace = Vec<(SId, SType)>;

pub fn models(T: &Trace, C: &Ctx) -> Option<CtxCtx> {
    None
}

pub fn check(ctx: &Ctx, e: &SExpr, t: &SType) -> Result<(Eff, Ctx), TypeError> {
    Ok((Eff::No, Ctx::Empty))
}

// TODO: needs to be done inline because we don't have polymorphism
pub fn const_type(c: &Const) -> Type {
    match c {
        Const::Unit => Type::Unit,
        Const::New(r) => Type::Regex(r.clone()),
        Const::Write(w) => {
            let r = ();
            Type::Arr(
                fake_span(Mult::Unr),
                fake_span(Eff::Yes),
                Box::new(fake_span(Type::Regex(fake_span(r)))),
                Box::new(fake_span(Type::Regex(fake_span(r)))),
            )
        }
        Const::Split(r) => todo!(),
        Const::Close => todo!(),
    }
}

pub fn fake_span<T>(t: T) -> Spanned<T> {
    Spanned::new(t, 0..0)
}

pub fn infer(ctx: &Ctx, e: &SExpr) -> Result<(Type, Eff, Ctx), TypeError> {
    match &e.val {
        Expr::Const(c) => Ok((const_type(c), Eff::No, ctx.clone())),
        Expr::Loc(l) => Err(TypeError::LocationExpr(l.clone())),
        Expr::Var(x) => match ctx.lookup_ord_pure(x) {
            Some((ctx, t)) => Ok((t, Eff::No, ctx)),
            None => Err(TypeError::UndefinedVariable(x.clone())),
        },
        Expr::Abs(m, x, e) => todo!(),
        Expr::App(m, e1, e2) => {
            // TODO: for unordered, we need to check that ctx models (((ctx - c1) || (c1 - c2)) , c2) ...
            let (t1, p1, c1) = infer(ctx, e1)?;
            let (t2, p2, c2) = infer(&c1, e2)?;
            match t1 {
                Type::Arr(m, p, t21, t22) if t21.val == t2 => {
                    Ok((t22.val, lub(p.val, lub(p1, p2)), c2))
                }
                _ => todo!(),
            }
        }
        Expr::Pair(m, e1, e2) => {
            let (t1, p1, c1) = infer(ctx, e1)?;
            let (t2, p2, c2) = infer(&c1, e2)?;
            Ok((
                Type::Prod(m.clone(), Box::new(fake_span(t1)), Box::new(fake_span(t2))),
                lub(p1, p2),
                c2,
            ))
        }
        Expr::Let(m, x, y, e1, e2) => todo!(),
        Expr::Ann(e, t) => {
            let (eff, ctx) = check(ctx, e, t)?;
            Ok((t.val.clone(), eff, ctx))
        }
    }
}

pub fn infer_type(e: &SExpr) -> Result<(Type, Eff), TypeError> {
    let (t, e, C) = infer(&Ctx::Empty, e)?;
    Ok((t, e))
}
