#[cfg(test)]
mod tests {
    use crate::regex::*;

    #[test]
    fn test_accepts() {
        let e = or(seq(char('a'), char('b')), char('c'));
        assert!(e.accepts("ab".chars()));
        assert!(e.accepts("c".chars()));
        assert!(!e.accepts("a".chars()));
        assert!(!e.accepts("b".chars()));
        assert!(!e.accepts("abb".chars()));
        assert!(!e.accepts("cc".chars()));
    }

    #[test]
    fn test_deriv_re_1() {
        let e1 = or(seq(char('a'), char('b')), char('c'));
        let e2 = char('a');
        let e = e1.deriv_re(&e2);
        let e = e.simplify();
        eprintln!("{}", e);
        assert_eq!(e, char('b'));
    }

    #[test]
    fn test_deriv_re_2() {
        let e1 = or(seq(char('a'), char('b')), char('c'));
        let e2 = seq(char('a'), char('b'));
        let e = e1.deriv_re(&e2);
        let e = e.simplify();
        eprintln!("{}", e);
        assert_eq!(e, eps);
    }

    #[test]
    fn test_deriv_re_3() {
        let e1 = or(seq(char('a'), char('b')), char('c'));
        let e2 = char('c');
        let e = e1.deriv_re(&e2);
        let e = e.simplify();
        eprintln!("{}", e);
        assert_eq!(e, eps);
    }

    #[test]
    fn test_deriv_re_4() {
        let e1 = or(seq(char('a'), char('b')), char('c'));
        let e2 = char('d');
        let e = e1.deriv_re(&e2);
        let e = e.simplify();
        eprintln!("{}", e);
        assert_eq!(e, empty);
    }

    #[test]
    fn test_deriv_re_5() {
        let e1 = star(char('a'));
        let e2 = char('a');
        let e = e1.deriv_re(&e2);
        let e = e.simplify();
        eprintln!("{}", e);
        assert_eq!(e, star(char('a')));
    }

    #[test]
    fn test_deriv_re_6() {
        let e1 = star(seq(char('a'), char('b')));
        let e2 = char('a');
        let e = e1.deriv_re(&e2);
        let e = e.simplify();
        eprintln!("{}", e);
        assert_eq!(e, seq(char('b'), star(seq(char('a'), char('b')))));
    }

    #[test]
    fn test_re_to_dfa_to_re_1() {
        let e1 = star(seq(char(b'a'), char(b'b')));
        // eprintln!("{}", e1.to_dfa().minimized());
        // assert_eq!(e1, e1.to_dfa().to_regex().simplify());
        assert!(e1.is_equal_to(&e1.to_dfa().to_regex()));
    }
}
