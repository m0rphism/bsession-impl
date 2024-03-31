use crate::{
    error_reporting::{report_error, IErr},
    syntax::{Eff, Type},
    typecheck,
};

pub fn typecheck_(src: &str) -> Result<(Type, Eff), IErr> {
    typecheck(src).map_err(|e| {
        report_error("<nofile>", &src, e.clone());
        e
    })
}

#[test]
fn ty_id_unr_ann() {
    let src = "λ[unr] x. x : Unit -[ unr; pure ]→ Unit";
    // let src = "λ[unr] x. x : Unit -[u0]→ Unit";
    // let src = "λ[unr] x. x : Unit -[u;0]→ Unit";
    // let src = "λ[unr] x. x : Unit →u0 Unit";
    assert!(typecheck_(src).is_ok());
}

#[test]
fn ty_id_unr() {
    let src = "λx. x : Unit -[ unr; pure ]→ Unit";
    assert!(typecheck_(src).is_ok());
}

#[test]
fn ty_res_simple() {
    let src = "close (! 'x' (new {x})) : Unit";
    assert!(typecheck_(src).is_ok());
}

#[test]
fn ty_res_let() {
    let src = "let x, y = split {a} (new {ab}) in close (!'a' x); close (!'b' y) : Unit";
    assert!(typecheck_(src).is_ok());
}
