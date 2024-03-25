#[cfg(test)]
mod tests {
    use crate::regex::*;

    #[test]
    fn test_accepts() {
        let e = or(seq(char_('a'), char_('b')), char_('c'));
        assert!(e.accepts("ab".chars()));
        assert!(e.accepts("c".chars()));
        assert!(!e.accepts("a".chars()));
        assert!(!e.accepts("b".chars()));
        assert!(!e.accepts("abb".chars()));
        assert!(!e.accepts("cc".chars()));
    }

    #[test]
    fn test_deriv_re_1() {
        let e1 = or(seq(char_('a'), char_('b')), char_('c'));
        let e2 = char_('a');
        let e = e1.deriv_re(&e2);
        let e = e.simplify();
        eprintln!("{}", e);
        assert_eq!(e, char_('b'));
    }

    #[test]
    fn test_deriv_re_2() {
        let e1 = or(seq(char_('a'), char_('b')), char_('c'));
        let e2 = seq(char_('a'), char_('b'));
        let e = e1.deriv_re(&e2);
        let e = e.simplify();
        eprintln!("{}", e);
        assert_eq!(e, eps);
    }

    #[test]
    fn test_deriv_re_3() {
        let e1 = or(seq(char_('a'), char_('b')), char_('c'));
        let e2 = char_('c');
        let e = e1.deriv_re(&e2);
        let e = e.simplify();
        eprintln!("{}", e);
        assert_eq!(e, eps);
    }

    #[test]
    fn test_deriv_re_4() {
        let e1 = or(seq(char_('a'), char_('b')), char_('c'));
        let e2 = char_('d');
        let e = e1.deriv_re(&e2);
        let e = e.simplify();
        eprintln!("{}", e);
        assert_eq!(e, empty);
    }

    #[test]
    fn test_deriv_re_5() {
        let e1 = star(char_('a'));
        let e2 = char_('a');
        let e = e1.deriv_re(&e2);
        let e = e.simplify();
        eprintln!("{}", e);
        assert_eq!(e, star(char_('a')));
    }

    #[test]
    fn test_deriv_re_6() {
        let e1 = star(seq(char_('a'), char_('b')));
        let e2 = char_('a');
        let e = e1.deriv_re(&e2);
        let e = e.simplify();
        eprintln!("{}", e);
        assert_eq!(e, seq(char_('b'), star(seq(char_('a'), char_('b')))));
    }

    #[test]
    fn test_re_to_dfa_to_re_1() {
        let e1 = star(seq(char_(b'a'), char_(b'b')));
        // eprintln!("{}", e1.to_dfa().minimized());
        // assert_eq!(e1, e1.to_dfa().to_regex().simplify());
        assert!(e1.is_equal_to(&e1.to_dfa().to_regex()));
    }
}
