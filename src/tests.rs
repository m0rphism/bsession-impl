use crate::{
    error_reporting::{report_error, IErr},
    syntax::{Eff, Type},
    typecheck,
};

#[test]
fn id_unr() {
    let src = "λ[unr] x. x : Unit -[ unr; pure ]→ Unit";
    assert!(typecheck(src).is_ok());
}

#[test]
fn res_simple() {
    let src = "close (! 'x' (new {x})) : Unit";
    assert!(typecheck(src).is_ok());
}

pub fn typecheck_(src: &str) -> Result<(Type, Eff), IErr> {
    typecheck(src).map_err(|e| {
        report_error("<nofile>", &src, e.clone());
        e
    })
}

#[test]
fn res_let() {
    let src = "let x ,[left] y = split {a} (new {ab}) in close x : Unit";
    let res = typecheck_(src);
    assert!(res.is_ok(), "Typechecking failed! Error: {res:?}");
}
