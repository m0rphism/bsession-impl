use crate::{
    regex::Regex,
    syntax::{Eff, Expr, Id, Mult, SEff, SExpr, SId, SLoc, SMult, SRegex, SType, SWord, Type},
    type_context::{Ctx, CtxCtx, CtxS, JoinOrd},
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
    CtxSplitFailed(SExpr, Ctx, Ctx),
    CtxCtxSplitFailed(Spanned<Expr>, Ctx, std::collections::HashSet<String>),
}

pub fn check(ctx: &Ctx, e: &SExpr, t: &SType) -> Result<Eff, TypeError> {
    match &e.val {
        Expr::Loc(l) => Err(TypeError::LocationExpr(l.clone())),
        Expr::Abs(m, x, e_body) => match &t.val {
            Type::Arr(m2, _p, _t1, _t2) if m.val != m2.val => {
                Err(TypeError::MismatchMult(m.clone(), m2.clone()))
            }
            Type::Arr(_m2, p, t1, t2) => {
                let ctx2 = match m.val {
                    Mult::Unr => CtxS::Join(
                        ctx,
                        Ctx::Bind(x.clone(), t1.as_ref().clone()),
                        JoinOrd::Ordered,
                    ),
                    Mult::Lin => CtxS::Join(
                        ctx,
                        Ctx::Bind(x.clone(), t1.as_ref().clone()),
                        JoinOrd::Unordered,
                    ),
                    Mult::OrdL => CtxS::Join(
                        ctx,
                        Ctx::Bind(x.clone(), t1.as_ref().clone()),
                        JoinOrd::Ordered,
                    ),
                    Mult::OrdR => CtxS::Join(
                        Ctx::Bind(x.clone(), t1.as_ref().clone()),
                        ctx,
                        JoinOrd::Ordered,
                    ),
                };
                let po = check(&ctx2, e_body, t2)?;
                if Eff::leq(po, p.val) {
                    Ok(Eff::No)
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
        _ => {
            let (t2, p) = infer(ctx, e)?;
            if t.val.is_equal_to(&t2.val) {
                Ok(p)
            } else {
                Err(TypeError::Mismatch(t.clone(), t2))
            }
        }
    }
}

pub fn infer(ctx: &Ctx, e: &SExpr) -> Result<(SType, Eff), TypeError> {
    match &e.val {
        Expr::Unit => Ok((fake_span(Type::Unit), Eff::No)),
        Expr::New(r) => Ok((fake_span(Type::Regex(r.clone())), Eff::No)),
        Expr::Write(w, e) => {
            let (t, _p) = infer(ctx, e)?;
            match &t.val {
                Type::Regex(r) => {
                    let r2 = r.deriv_word(w.as_bytes().iter().cloned());
                    if r2.is_empty() {
                        Err(TypeError::InvalidWrite(r.clone(), w.clone()))
                    } else {
                        Ok((fake_span(Type::Regex(fake_span(r2))), Eff::Yes))
                    }
                }
                _ => Err(TypeError::Mismatch(
                    fake_span(Type::Regex(fake_span(Regex::Eps))),
                    t.clone(),
                )),
            }
        }
        Expr::Split(r1, e) => {
            let (t, _p) = infer(ctx, e)?;
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
            let (t, p) = infer(ctx, e)?;
            match &t.val {
                Type::Regex(r) if r.nullable() => Ok((fake_span(Type::Unit), p)),
                Type::Regex(r) => Err(TypeError::ClosedUnfinished(e.as_ref().clone(), r.clone())),
                _ => Err(TypeError::Mismatch(
                    fake_span(Type::Regex(fake_span(Regex::Eps))),
                    t.clone(),
                )),
            }
        }
        Expr::Loc(l) => Err(TypeError::LocationExpr(l.clone())),
        Expr::Var(x) => match ctx.lookup_ord_pure(x) {
            Some((ctx, t)) => Ok((t.clone(), Eff::No)),
            None => Err(TypeError::UndefinedVariable(x.clone())),
        },
        Expr::Abs(_m, _x, _e) => Err(TypeError::TypeAnnotationMissing(e.clone())),
        Expr::App(m, e1, e2) => {
            let c1 = ctx.restrict(&e1.free_vars());
            let c2 = ctx.restrict(&e2.free_vars());
            let (t1, p1) = infer(&c1, e1)?;
            let (t2, p2) = infer(&c2, e2)?;
            let c12 = match m.val {
                Mult::Unr => CtxS::Join(c1.clone(), c2.clone(), JoinOrd::Ordered),
                Mult::Lin => CtxS::Join(c1.clone(), c2.clone(), JoinOrd::Unordered),
                Mult::OrdL => CtxS::Join(c1.clone(), c2.clone(), JoinOrd::Ordered),
                Mult::OrdR => CtxS::Join(c2.clone(), c1.clone(), JoinOrd::Ordered),
            };
            if !ctx.is_subctx_of(&c12) {
                Err(TypeError::CtxSplitFailed(
                    e.clone(),
                    ctx.clone(),
                    c12.clone(),
                ))?
            }
            match t1.val {
                Type::Arr(m2, _p, _t11, _t12) if m.val != m2.val => {
                    Err(TypeError::MismatchMult(m.clone(), m2.clone()))
                }
                Type::Arr(_m2, p, t11, t12) if t11.val.is_equal_to(&t2.val) => {
                    Ok((*t12, Eff::lub(p.val, Eff::lub(p1, p2))))
                }
                _ => Err(TypeError::Mismatch(t1.clone(), t2.clone())),
            }
        }
        Expr::Pair(m, e1, e2) => {
            let c1 = ctx.restrict(&e1.free_vars());
            let c2 = ctx.restrict(&e2.free_vars());
            let (t1, p1) = infer(&c1, e1)?;
            let (t2, p2) = infer(&c2, e2)?;

            let c12 = match m.val {
                Mult::Unr => CtxS::Join(c1.clone(), c2.clone(), JoinOrd::Ordered),
                Mult::Lin => CtxS::Join(c1.clone(), c2.clone(), JoinOrd::Unordered),
                Mult::OrdL => CtxS::Join(c1.clone(), c2.clone(), JoinOrd::Ordered),
                Mult::OrdR => CtxS::Join(c2.clone(), c1.clone(), JoinOrd::Ordered),
            };
            if !ctx.is_subctx_of(&c12) {
                Err(TypeError::CtxSplitFailed(
                    e.clone(),
                    ctx.clone(),
                    c12.clone(),
                ))?
            }

            Ok((
                fake_span(Type::Prod(m.clone(), Box::new(t1), Box::new(t2))),
                Eff::lub(p1, p2),
            ))
        }
        Expr::Let(m, x, y, e1, e2) => {
            let (cc, c) = match ctx.split(&e1.free_vars()) {
                Some(Some((cc, c))) => (cc, c),
                Some(None) => (CtxCtx::Hole, ctx.clone()),
                None => Err(TypeError::CtxCtxSplitFailed(
                    e.clone(),
                    ctx.clone(),
                    e1.free_vars(),
                ))?,
            };
            let (t1, p1) = infer(&c, e1)?;
            let (t11, t12) = match t1.val {
                Type::Prod(m2, t11, t12) if m.val != m2.val => {
                    Err(TypeError::MismatchMult(m.clone(), m2.clone()))?
                }
                Type::Prod(m2, t11, t12) => (*t11, *t12),
                _ => Err(TypeError::Mismatch(
                    fake_span(Type::Prod(
                        m.clone(),
                        Box::new(fake_span(Type::Unit)),
                        Box::new(fake_span(Type::Unit)),
                    )),
                    t1,
                ))?,
            };
            let c_fill = match m.val {
                Mult::Unr => CtxS::Join(
                    CtxS::Bind(x.clone(), t11),
                    CtxS::Bind(y.clone(), t12),
                    JoinOrd::Ordered,
                ),
                Mult::Lin => CtxS::Join(
                    CtxS::Bind(x.clone(), t11),
                    CtxS::Bind(y.clone(), t12),
                    JoinOrd::Unordered,
                ),
                Mult::OrdL => CtxS::Join(
                    CtxS::Bind(x.clone(), t11),
                    CtxS::Bind(y.clone(), t12),
                    JoinOrd::Ordered,
                ),
                Mult::OrdR => CtxS::Join(
                    CtxS::Bind(y.clone(), t12),
                    CtxS::Bind(x.clone(), t11),
                    JoinOrd::Ordered,
                ),
            };
            let c2 = cc.fill(c_fill);
            infer(&c2, e2)
        }
        Expr::Ann(e, t) => {
            let eff = check(ctx, e, t)?;
            Ok((t.clone(), eff))
        }
    }
}

pub fn infer_type(e: &SExpr) -> Result<(SType, Eff), TypeError> {
    infer(&Ctx::Empty, e)
}
