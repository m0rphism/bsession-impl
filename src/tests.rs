use crate::typecheck;

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
