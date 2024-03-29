use crate::{
    regex::Regex,
    syntax::{Eff, Expr, Id, Mult, SEff, SExpr, SId, SLoc, SMult, SRegex, SType, SWord, Type},
    type_context::{Ctx, JoinOrd},
    util::span::{fake_span, Spanned},
};

#[derive(Debug, Clone)]
pub enum TypeError {
    LocationExpr(SLoc),
    UndefinedVariable(SId),
    LeftOver(Ctx),
    OrdReturn(SType),
    Mismatch(SType, SType),
    MismatchMult(SMult, SMult),
    MismatchEff(SEff, SEff),
    TypeAnnotationMissing(SExpr),
    ClosedUnfinished(SExpr, SRegex),
    InvalidWrite(SRegex, SWord),
    InvalidSplitArg(SRegex, SRegex),
    InvalidSplitRes(SRegex, SRegex),
}

pub fn check(ctx: &Ctx, e: &SExpr, t: &SType) -> Result<(Eff, Ctx, Ctx), TypeError> {
    match &e.val {
        Expr::Loc(l) => Err(TypeError::LocationExpr(l.clone())),
        Expr::Abs(m, x, e_body) => match &t.val {
            Type::Arr(m2, _p, _t1, _t2) if m.val != m2.val => {
                Err(TypeError::MismatchMult(m.clone(), m2.clone()))
            }
            Type::Arr(_m2, p, t1, t2) => {
                let ctx2 = match m.val {
                    Mult::Unr => Ctx::Join(
                        Box::new(ctx.clone()),
                        Box::new(Ctx::Bind(x.clone(), t1.as_ref().clone())),
                        JoinOrd::Ordered,
                    ),
                    Mult::Lin => Ctx::Join(
                        Box::new(ctx.clone()),
                        Box::new(Ctx::Bind(x.clone(), t1.as_ref().clone())),
                        JoinOrd::Unordered,
                    ),
                    Mult::OrdL => Ctx::Join(
                        Box::new(ctx.clone()),
                        Box::new(Ctx::Bind(x.clone(), t1.as_ref().clone())),
                        JoinOrd::Ordered,
                    ),
                    Mult::OrdR => Ctx::Join(
                        Box::new(Ctx::Bind(x.clone(), t1.as_ref().clone())),
                        Box::new(ctx.clone()),
                        JoinOrd::Ordered,
                    ),
                };
                let (po, co1, co2) = check(&ctx2, e_body, t2)?;
                if Eff::leq(po, p.val) {
                    Ok((Eff::No, co1, co2))
                } else {
                    Err(TypeError::MismatchEff(p.clone(), fake_span(po)))
                }
            }
            _ => Err(TypeError::Mismatch(
                fake_span(Type::Arr(
                    m.clone(),
                    fake_span(Eff::Yes),
                    Box::new(fake_span(Type::Unit)),
                    Box::new(fake_span(Type::Unit)),
                )),
                t.clone(),
            )),
        },
        Expr::Let(_, _, _, _, _) => todo!(),
        _ => {
            let (t2, p, c1, c2) = infer(ctx, e)?;
            if t.val.is_equal_to(&t2.val) {
                Ok((p, c1, c2))
            } else {
                Err(TypeError::Mismatch(t.clone(), t2))
            }
        }
    }
}

pub fn infer(ctx: &Ctx, e: &SExpr) -> Result<(SType, Eff, Ctx, Ctx), TypeError> {
    match &e.val {
        Expr::Unit => Ok((fake_span(Type::Unit), Eff::No, Ctx::Empty, ctx.clone())),
        Expr::New(r) => Ok((
            fake_span(Type::Regex(r.clone())),
            Eff::No,
            Ctx::Empty,
            ctx.clone(),
        )),
        Expr::Write(w, e) => {
            let (t, _p, c1, c2) = infer(ctx, e)?;
            match &t.val {
                Type::Regex(r) => {
                    let r2 = r.deriv_word(w.as_bytes().iter().cloned());
                    if r2.is_empty() {
                        Err(TypeError::InvalidWrite(r.clone(), w.clone()))
                    } else {
                        Ok((fake_span(Type::Regex(fake_span(r2))), Eff::Yes, c1, c2))
                    }
                }
                _ => Err(TypeError::Mismatch(
                    fake_span(Type::Regex(fake_span(Regex::Eps))),
                    t.clone(),
                )),
            }
        }
        Expr::Split(r1, e) => {
            let (t, _p, c1, c2) = infer(ctx, e)?;
            match &t.val {
                Type::Regex(r) => {
                    let r2 = r.deriv_re(r1);
                    if r1.is_empty() {
                        Err(TypeError::InvalidSplitArg(r.clone(), r1.clone()))
                    } else if r2.is_empty() {
                        Err(TypeError::InvalidSplitRes(r.clone(), r1.clone()))
                    } else {
                        Ok((
                            fake_span(Type::Prod(
                                fake_span(Mult::OrdL),
                                Box::new(fake_span(Type::Regex(r1.clone()))),
                                Box::new(fake_span(Type::Regex(fake_span(r2)))),
                            )),
                            Eff::Yes,
                            c1,
                            c2,
                        ))
                    }
                }
                _ => Err(TypeError::Mismatch(
                    fake_span(Type::Regex(fake_span(Regex::Eps))),
                    t.clone(),
                )),
            }
        }
        Expr::Close(e) => {
            let (t, p, c1, c2) = infer(ctx, e)?;
            match &t.val {
                Type::Regex(r) if r.nullable() => Ok((fake_span(Type::Unit), p, c1, c2)),
                Type::Regex(r) => Err(TypeError::ClosedUnfinished(e.as_ref().clone(), r.clone())),
                _ => Err(TypeError::Mismatch(
                    fake_span(Type::Regex(fake_span(Regex::Eps))),
                    t.clone(),
                )),
            }
        }
        Expr::Loc(l) => Err(TypeError::LocationExpr(l.clone())),
        Expr::Var(x) => match ctx.lookup_ord_pure(x) {
            Some((ctx, t)) => Ok((t.clone(), Eff::No, Ctx::Bind(x.clone(), t.clone()), ctx)),
            None => Err(TypeError::UndefinedVariable(x.clone())),
        },
        Expr::Abs(_m, _x, _e) => Err(TypeError::TypeAnnotationMissing(e.clone())),
        Expr::App(m, e1, e2) => {
            // TODO: for linear, we need to check that ctx models (((ctx - c1) || (c1 - c2)) , c2) ...
            match m.val {
                Mult::OrdR => {
                    let (t2, p2, c21, c22) = infer(ctx, e2)?;
                    let (t1, p1, c11, c12) = infer(&c22, e1)?;
                    match &t1.val {
                        Type::Arr(m2, _p, _t11, _t12) if m != m2 => {
                            Err(TypeError::MismatchMult(m.clone(), m2.clone()))
                        }
                        Type::Arr(_m2, p, t11, t12) if t11.val == t2.val => Ok((
                            t12.as_ref().clone(),
                            Eff::lub(p.val, Eff::lub(p1, p2)),
                            Ctx::Join(Box::new(c21), Box::new(c11), m.to_join_ord()),
                            c12,
                        )),
                        _ => Err(TypeError::Mismatch(t1.clone(), t2.clone())),
                    }
                }
                _ => {
                    let (t1, p1, c11, c12) = infer(ctx, e1)?;
                    let (t2, p2, c21, c22) = infer(&c12, e2)?;
                    match &t1.val {
                        Type::Arr(m2, _p, _t11, _t12) if m != m2 => {
                            Err(TypeError::MismatchMult(m.clone(), m2.clone()))
                        }
                        Type::Arr(_m2, p, t11, t12) if t11.val == t2.val => Ok((
                            t12.as_ref().clone(),
                            Eff::lub(p.val, Eff::lub(p1, p2)),
                            Ctx::Join(Box::new(c11), Box::new(c21), m.to_join_ord()),
                            c22,
                        )),
                        _ => Err(TypeError::Mismatch(t1.clone(), t2.clone())),
                    }
                }
            }
        }
        Expr::Pair(m, e1, e2) => {
            let (t1, p1, c11, c12) = infer(ctx, e1)?;
            let (t2, p2, c21, c22) = infer(&c12, e2)?;
            // TODO: if m is linear, check used contexts
            Ok((
                fake_span(Type::Prod(m.clone(), Box::new(t1), Box::new(t2))),
                Eff::lub(p1, p2),
                Ctx::Join(Box::new(c11), Box::new(c21), m.to_join_ord()),
                c22,
            ))
        }
        Expr::Let(_m, _x, _y, _e1, _e2) => Err(TypeError::TypeAnnotationMissing(e.clone())),
        Expr::Ann(e, t) => {
            let (eff, c1, c2) = check(ctx, e, t)?;
            Ok((t.clone(), eff, c1, c2))
        }
    }
}

pub fn infer_type(e: &SExpr) -> Result<(SType, Eff), TypeError> {
    let (t, e, _c1, c2) = infer(&Ctx::Empty, e)?;
    if !t.is_unr() {
        Err(TypeError::OrdReturn(t))
    } else if !c2.is_unr() {
        Err(TypeError::LeftOver(c2))
    } else {
        Ok((t, e))
    }
}
