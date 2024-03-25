#[cfg(test)]
mod tests {
    use crate::typecheck;

    #[test]
    fn test_id_unr() {
        let src = "λ[unr] x. x : Unit -[ unr; pure ]→ Unit";
        assert!(typecheck(src).is_ok());
    }

    #[test]
    fn test_res_simple() {
        let src = "close (! 'x' (new {x})) : Unit";
        assert!(typecheck(src).is_ok());
    }
}
