use std::collections::HashSet;

use crate::{
    syntax::{Eff, Expr, Id, Mult, SEff, SExpr, SId, SLoc, SMult, SRegex, SType, Type},
    type_context::{Ctx, CtxCtx, CtxS, JoinOrd},
    util::{pretty::pretty_def, span::fake_span},
};

#[derive(Debug, Clone)]
pub enum TypeError {
    LocationExpr(SLoc),
    UndefinedVariable(SId),
    Mismatch(SExpr, Result<SType, String>, SType),
    MismatchMult(SExpr, SType, SMult, SMult),
    MismatchEff(SExpr, SEff, SEff),
    MismatchEffSub(SExpr, SEff, SEff),
    TypeAnnotationMissing(SExpr),
    ClosedUnfinished(SExpr, SRegex),
    InvalidWrite(SExpr, SRegex, SRegex),
    InvalidSplitArg(SRegex),
    InvalidSplitRes(SExpr, SRegex, SRegex, SRegex),
    CtxSplitFailed(SExpr, Ctx, Ctx),
    CtxCtxSplitFailed(SExpr, Ctx, HashSet<Id>),
    Shadowing(SExpr, SId),
    CtxNotUnr(SExpr, Ctx),
    NewEmpty(SRegex),
    SeqDropsOrd(SExpr, SType),
    LeftOverCtx(SExpr, Ctx),
}

// TODO: Subtyping

pub fn check(ctx: &Ctx, e: &SExpr, t: &SType) -> Result<Eff, TypeError> {
    match &e.val {
        Expr::Loc(l) => Err(TypeError::LocationExpr(l.clone())),
        Expr::Abs(om, x, e_body) => match &t.val {
            Type::Arr(m, p, t1, t2) => {
                if let Some(m2) = om {
                    if m.val != m2.val {
                        Err(TypeError::MismatchMult(
                            e.clone(),
                            t.clone(),
                            m.clone(),
                            m2.clone(),
                        ))?
                    }
                }

                if m.val == Mult::Unr && !ctx.is_unr() {
                    Err(TypeError::CtxNotUnr(e.clone(), ctx.clone()))?
                }
                if ctx.vars().contains(&x.val) {
                    Err(TypeError::Shadowing(e.clone(), x.clone()))?
                }
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
                    Err(TypeError::MismatchEffSub(
                        *e_body.clone(),
                        p.clone(),
                        fake_span(po),
                    ))
                }
            }
            _ => Err(TypeError::Mismatch(
                e.clone(),
                Err(format!("function type")),
                t.clone(),
            )),
        },
        _ => {
            let (t2, p) = infer(ctx, e)?;
            if t.val.is_equal_to(&t2.val) {
                Ok(p)
            } else {
                Err(TypeError::Mismatch(e.clone(), Ok(t.clone()), t2))
            }
        }
    }
}

fn fresh_var() -> SId {
    static mut NEXT_FRESH_VAR: usize = 0;
    let n = unsafe {
        NEXT_FRESH_VAR += 1;
        NEXT_FRESH_VAR - 1
    };
    fake_span(format!("FRESH_{}", n))
}

pub fn assert_unr_ctx(e: &SExpr, ctx: &Ctx) -> Result<(), TypeError> {
    if ctx.is_unr() {
        Ok(())
    } else {
        Err(TypeError::LeftOverCtx(e.clone(), ctx.clone()))
    }
}

pub fn infer(ctx: &Ctx, e: &SExpr) -> Result<(SType, Eff), TypeError> {
    match &e.val {
        Expr::Unit => {
            assert_unr_ctx(e, &ctx)?;
            Ok((fake_span(Type::Unit), Eff::No))
        }
        Expr::New(r) if r.is_empty() => Err(TypeError::NewEmpty(r.clone()))?,
        Expr::New(r) => {
            assert_unr_ctx(e, &ctx)?;
            Ok((fake_span(Type::Regex(r.clone())), Eff::No))
        }
        Expr::Write(w, e2) => {
            let (t, _p) = infer(ctx, e2)?;
            match &t.val {
                Type::Regex(r) => {
                    let r2 = r.deriv_re_norm(&w.val);
                    if r2.is_empty() {
                        Err(TypeError::InvalidWrite(e.clone(), r.clone(), w.clone()))
                    } else {
                        Ok((fake_span(Type::Regex(fake_span(r2))), Eff::Yes))
                    }
                }
                _ => Err(TypeError::Mismatch(
                    e.clone(),
                    Err(format!("resource type")),
                    t.clone(),
                )),
            }
        }
        Expr::Split(r1, e2) => {
            let (t, p) = infer(ctx, e2)?;
            match &t.val {
                Type::Regex(r) => {
                    let r2 = r.deriv_re_norm(r1);
                    if r1.is_empty() {
                        Err(TypeError::InvalidSplitArg(r1.clone()))
                    } else if r2.is_empty() {
                        Err(TypeError::InvalidSplitRes(
                            e.clone(),
                            r.clone(),
                            r1.clone(),
                            fake_span(r2.clone()),
                        ))
                    } else {
                        Ok((
                            fake_span(Type::Prod(
                                fake_span(Mult::OrdL),
                                Box::new(fake_span(Type::Regex(r1.clone()))),
                                Box::new(fake_span(Type::Regex(fake_span(r2)))),
                            )),
                            p,
                        ))
                    }
                }
                _ => Err(TypeError::Mismatch(
                    e.clone(),
                    Err(format!("resource type")),
                    t.clone(),
                )),
            }
        }
        Expr::Drop(e2) => {
            let (t, p) = infer(ctx, e2)?;
            match &t.val {
                Type::Regex(r) if r.nullable() => Ok((fake_span(Type::Unit), p)),
                Type::Regex(r) => Err(TypeError::ClosedUnfinished(e2.as_ref().clone(), r.clone())),
                _ => Err(TypeError::Mismatch(
                    e.clone(),
                    Err(format!("resource type")),
                    t.clone(),
                )),
            }
        }
        Expr::Loc(l) => Err(TypeError::LocationExpr(l.clone())),
        // TODO: This check is not necessary anymore, because contexts
        // contain exactly the free variables.
        Expr::Var(x) => match ctx.lookup_ord_pure(x) {
            Some((ctx, t)) => {
                assert_unr_ctx(e, &ctx)?;
                Ok((t.clone(), Eff::No))
            }
            None => Err(TypeError::UndefinedVariable(x.clone())),
        },
        Expr::Abs(_m, _x, _e) => Err(TypeError::TypeAnnotationMissing(e.clone())),
        Expr::App(e1, e2) => {
            let c1 = ctx.restrict(&e1.free_vars());
            let c2 = ctx.restrict(&e2.free_vars());
            let (t1, p1) = infer(&c1, e1)?;
            let (t2, p2) = infer(&c2, e2)?;
            match t1.val {
                Type::Arr(m, p, t11, t12) if t11.val.is_equal_to(&t2.val) => {
                    match m.val {
                        Mult::OrdL if p2 == Eff::Yes => Err(TypeError::MismatchEff(
                            *e2.clone(),
                            fake_span(Eff::No),
                            fake_span(p2),
                        ))?,
                        Mult::OrdR if p1 == Eff::Yes => Err(TypeError::MismatchEff(
                            *e1.clone(),
                            fake_span(Eff::No),
                            fake_span(p1),
                        ))?,
                        _ => (),
                    }
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
                    Ok((*t12, Eff::lub(p.val, Eff::lub(p1, p2))))
                }
                _ => Err(TypeError::Mismatch(e.clone(), Ok(t1.clone()), t2.clone())),
            }
        }
        Expr::AppBorrow(_e1, _x) => {
            panic!("Borrows are not supported, yet")
            ////////////////////////////////////////////////////////////////////////////////
            // Translating borrow notation is nonlocal:
            //   The code

            //     let x = new {ab} in (f &x; close (!b) x)

            //   needs to be translated to

            //     let x = new {ab} in let (y , x) = split a x in (f y; close (!b) x)

            //   instead of

            //     let x = new {ab} in (let (y , x) = split a x in f y); close (!b) x

            //   We cannot do it as a preprocessing because, we need type checker information.
            //   We cannot do it directly in the typechecker without changing the shape of the relation.
            ////////////////////////////////////////////////////////////////////////////////

            // let c1 = {
            //     let mut xs = ctx.vars();
            //     xs.remove(&x.val);
            //     ctx.restrict(&xs)
            // };
            // // let c2 = ctx.restrict(&HashSet::from([x.val.clone()]));

            // let (t, _) = infer(&c1, &e1)?;
            // let r = match &t.val {
            //     Type::Arr(_m, _p, t1, _t2) => match &t1.val {
            //         Type::Regex(r) => r.clone(),
            //         t => Err(TypeError::Mismatch(
            //             *e1.clone(),
            //             Err(format!("function type with resource domain")),
            //             fake_span(t.clone()),
            //         ))?,
            //     },
            //     t => Err(TypeError::Mismatch(
            //         *e1.clone(),
            //         Err(format!("function type")),
            //         fake_span(t.clone()),
            //     ))?,
            // };

            // let y = fresh_var();
            // let e_new = fake_span(Expr::LetPair(
            //     y.clone(),
            //     x.clone(),
            //     Box::new(fake_span(Expr::Split(
            //         r,
            //         Box::new(fake_span(Expr::Var(x.clone()))),
            //     ))),
            //     Box::new(fake_span(Expr::App(
            //         e1.clone(),
            //         Box::new(fake_span(Expr::Var(y))),
            //     ))),
            // ));
            // eprintln!("BORROW BORROW BORROW");
            // eprintln!("Expression In: {}", pretty_def(&e));
            // eprintln!("Expression Out: {}", pretty_def(&e_new));
            // eprintln!("Context: {}", pretty_def(&ctx.simplify()));
            // infer(ctx, &e_new)
        }
        Expr::Pair(om, e1, e2) => {
            let c1 = ctx.restrict(&e1.free_vars());
            let c2 = ctx.restrict(&e2.free_vars());
            let (t1, p1) = infer(&c1, e1)?;
            let (t2, p2) = infer(&c2, e2)?;

            let m = {
                let c12_lin = CtxS::Join(c1.clone(), c2.clone(), JoinOrd::Unordered);
                if ctx.is_subctx_of(&c12_lin) {
                    Mult::Lin
                } else {
                    let c12_ordl = CtxS::Join(c1.clone(), c2.clone(), JoinOrd::Ordered);
                    if ctx.is_subctx_of(&c12_ordl) {
                        Mult::OrdL
                    } else {
                        Err(TypeError::CtxSplitFailed(
                            e.clone(),
                            ctx.clone(),
                            c12_ordl.clone(),
                        ))?
                    }
                }
            };

            match m {
                Mult::OrdL if t1.is_ord() && p2 == Eff::Yes => Err(TypeError::MismatchEff(
                    *e2.clone(),
                    fake_span(Eff::No),
                    fake_span(p2),
                ))?,
                Mult::OrdR if t2.is_ord() && p1 == Eff::Yes => Err(TypeError::MismatchEff(
                    *e1.clone(),
                    fake_span(Eff::No),
                    fake_span(p1),
                ))?,
                _ => (),
            }

            let t_pair = fake_span(Type::Prod(fake_span(m), Box::new(t1), Box::new(t2)));

            if let Some(m2) = om {
                if m != m2.val {
                    Err(TypeError::MismatchMult(
                        e.clone(),
                        t_pair.clone(),
                        fake_span(m),
                        m2.clone(),
                    ))?
                }
            }

            Ok((t_pair, Eff::lub(p1, p2)))
        }
        Expr::LetPair(x, y, e1, e2) => {
            let (cc, c) = match ctx.split(&e1.free_vars()) {
                Some((cc, c)) => (cc, c),
                None => Err(TypeError::CtxCtxSplitFailed(
                    e.clone(),
                    ctx.clone(),
                    e1.free_vars(),
                ))?,
            };
            let (t1, p1) = infer(&c, e1)?;
            if p1 == Eff::Yes {
                Err(TypeError::MismatchEff(
                    *e1.clone(),
                    fake_span(Eff::No),
                    fake_span(p1),
                ))?
            }
            let (m, t11, t12) = match t1.val {
                Type::Prod(m, t11, t12) => (m, *t11, *t12),
                _ => Err(TypeError::Mismatch(
                    e.clone(),
                    Err(format!("product type")),
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

            let ctx_vars = cc.fill(Ctx::Empty).vars();
            if ctx_vars.contains(&x.val) {
                Err(TypeError::Shadowing(e.clone(), x.clone()))?
            }
            if ctx_vars.contains(&y.val) || x.val == y.val {
                Err(TypeError::Shadowing(e.clone(), y.clone()))?
            }

            let ctx2 = &cc.fill(c_fill);
            infer(ctx2, e2)
        }
        Expr::Ann(e, t) => {
            let eff = check(ctx, e, t)?;
            Ok((t.clone(), eff))
        }
        Expr::Let(x, e1, e2) => {
            let c1 = ctx.restrict(&e1.free_vars());
            let c2 = ctx.restrict(&e2.free_vars());

            if c2.vars().contains(&x.val) {
                Err(TypeError::Shadowing(e.clone(), x.clone()))?
            }

            let c12 = CtxS::Join(c1.clone(), c2.clone(), JoinOrd::Ordered);
            if !ctx.is_subctx_of(&c12) {
                Err(TypeError::CtxSplitFailed(
                    e.clone(),
                    ctx.clone(),
                    c12.clone(),
                ))?
            }

            let (t1, p1) = infer(&c1, e1)?;
            let c2x = CtxS::Join(CtxS::Bind(x.clone(), t1), &c2, JoinOrd::Ordered);
            let (t2, p2) = infer(&c2x, e2)?;
            Ok((t2, Eff::lub(p1, p2)))
        }
        Expr::Seq(e1, e2) => {
            let c1 = ctx.restrict(&e1.free_vars());
            let c2 = ctx.restrict(&e2.free_vars());

            let (t1, p1) = infer(&c1, e1)?;
            let (t2, p2) = infer(&c2, e2)?;

            if t1.is_ord() {
                Err(TypeError::SeqDropsOrd(e.clone(), t1.clone()))?
            }

            let c12 = CtxS::Join(c1.clone(), c2.clone(), JoinOrd::Ordered);
            if !ctx.is_subctx_of(&c12) {
                Err(TypeError::CtxSplitFailed(
                    e.clone(),
                    ctx.clone(),
                    c12.clone(),
                ))?
            }
            Ok((t2, Eff::lub(p1, p2)))
        }
    }
}

pub fn infer_type(e: &SExpr) -> Result<(SType, Eff), TypeError> {
    infer(&Ctx::Empty, e)
}
