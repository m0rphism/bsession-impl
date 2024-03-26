use crate::{regex::*, util::pretty::pretty_def};

#[test]
fn accepts() {
    let e = or(seq(char_('a'), char_('b')), char_('c'));
    assert!(e.accepts("ab".chars()));
    assert!(e.accepts("c".chars()));
    assert!(!e.accepts("a".chars()));
    assert!(!e.accepts("b".chars()));
    assert!(!e.accepts("abb".chars()));
    assert!(!e.accepts("cc".chars()));
}

#[test]
fn deriv_re_1() {
    let e1 = or(seq(char_('a'), char_('b')), char_('c'));
    let e2 = char_('a');
    let e = e1.deriv_re(&e2);
    let e = e.simplify();
    eprintln!("{}", e);
    assert_eq!(e, char_('b'));
}

#[test]
fn deriv_re_2() {
    let e1 = or(seq(char_('a'), char_('b')), char_('c'));
    let e2 = seq(char_('a'), char_('b'));
    let e = e1.deriv_re(&e2);
    let e = e.simplify();
    eprintln!("{}", e);
    assert_eq!(e, eps);
}

#[test]
fn deriv_re_3() {
    let e1 = or(seq(char_('a'), char_('b')), char_('c'));
    let e2 = char_('c');
    let e = e1.deriv_re(&e2);
    let e = e.simplify();
    eprintln!("{}", e);
    assert_eq!(e, eps);
}

#[test]
fn deriv_re_4() {
    let e1 = or(seq(char_('a'), char_('b')), char_('c'));
    let e2 = char_('d');
    let e = e1.deriv_re(&e2);
    let e = e.simplify();
    eprintln!("{}", e);
    assert_eq!(e, empty);
}

#[test]
fn deriv_re_5() {
    let e1 = star(char_('a'));
    let e2 = char_('a');
    let e = e1.deriv_re(&e2);
    let e = e.simplify();
    eprintln!("{}", e);
    assert_eq!(e, star(char_('a')));
}

#[test]
fn deriv_re_6() {
    let e1 = star(seq(char_('a'), char_('b')));
    let e2 = char_('a');
    let e = e1.deriv_re(&e2);
    let e = e.simplify();
    eprintln!("{}", e);
    assert_eq!(e, seq(char_('b'), star(seq(char_('a'), char_('b')))));
}

#[test]
fn re_to_dfa_to_re_1() {
    let e1 = star(seq(char_(b'a'), char_(b'b')));
    // eprintln!("{}", e1.to_dfa().minimized());
    // assert_eq!(e1, e1.to_dfa().to_regex().simplify());
    assert!(e1.is_equal_to(&e1.to_dfa().to_regex()));
}

// #[test]
// #[allow(non_snake_case)]
// fn random__to_dfa() {
//     eprintln!("–––––––––––––––––––––––");
//     for _ in 0..1000 {
//         let e = rand_regex(20);
//         eprintln!("{}", pretty_def(&e));
//         let _ = e.to_dfa();
//     }
//     eprintln!("–––––––––––––––––––––––");
// }

// #[test]
// #[allow(non_snake_case)]
// fn random__regex_to_dfa_to_regex() {
//     eprintln!("–––––––––––––––––––––––");

//     for _ in 0..500 {
//         let e = rand_regex(8);
//         let dfa = e.to_dfa().minimized();
//         let e2 = dfa.to_regex();
//         assert!(
//             e.is_equal_to(&e2),
//             "Not equal:\n  {}\n  {}\n  {}",
//             pretty_def(&e),
//             pretty_def(&e2),
//             dfa
//         );
//     }
//     eprintln!("–––––––––––––––––––––––");
// }

#[test]
fn foo() {
    // let e = Regex::<u8>::from_str("ab|a*c").unwrap();
    let e = Regex::<u8>::from_str("bb|a*ba").unwrap();

    let dfa = e.to_dfa().minimized();
    let e2 = dfa.to_regex();
    assert!(
        e.is_equal_to(&e2),
        "Not equal:\n  {}\n  {}\n  {}",
        pretty_def(&e),
        pretty_def(&e2),
        dfa
    );
}
// FAILS NON_DETERMINSICALLY!!
//
// Not equal:
//   bb|a*ba
//   (εb(b|a)|(εa)a*ba)ε
//   init: 0
// finals: {2}
// states:
//   0:
//     1: 98
//     4: 97
//   1:
//     2: 98
//     2: 97
//   2:
//   4:
//     4: 97
//     5: 98
//   5:
//     2: 97
