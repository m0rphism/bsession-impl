use std::collections::{BTreeSet, HashMap, HashSet, VecDeque};
use std::fmt::{Debug, Display};
use std::hash::{Hash, Hasher};

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Regex<C> {
    Empty,
    Eps,
    Char(C),
    Or(Box<Regex<C>>, Box<Regex<C>>),
    And(Box<Regex<C>>, Box<Regex<C>>),
    Seq(Box<Regex<C>>, Box<Regex<C>>),
    Star(Box<Regex<C>>),
    Neg(Box<Regex<C>>),
}

pub use Regex::Char as char;
pub use Regex::Empty as empty;
pub use Regex::Eps as eps;

use crate::pattern::{self, Example, Finite, Pattern, Realizable};
pub fn or<C>(e1: Regex<C>, e2: Regex<C>) -> Regex<C> {
    Regex::Or(Box::new(e1), Box::new(e2))
}
pub fn and<C>(e1: Regex<C>, e2: Regex<C>) -> Regex<C> {
    Regex::And(Box::new(e1), Box::new(e2))
}
pub fn seq<C>(e1: Regex<C>, e2: Regex<C>) -> Regex<C> {
    Regex::Seq(Box::new(e1), Box::new(e2))
}
pub fn star<C>(e: Regex<C>) -> Regex<C> {
    Regex::Star(Box::new(e))
}
pub fn neg<C>(e: Regex<C>) -> Regex<C> {
    Regex::Neg(Box::new(e))
}

impl<C: Display> Display for Regex<C> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Regex::Empty => write!(f, "∅"),
            Regex::Eps => write!(f, "ε"),
            Regex::Char(c) => write!(f, "{c}"),
            Regex::Or(e1, e2) => write!(f, "({e1}|{e2})"),
            Regex::And(e1, e2) => write!(f, "({e1}&{e2})"),
            Regex::Seq(e1, e2) => write!(f, "({e1}{e2})"),
            Regex::Star(e) => write!(f, "({e}*)"),
            Regex::Neg(e) => write!(f, "(~{e})"),
        }
    }
}

impl<C: Copy + Debug + Eq + Hash> Regex<C> {
    pub fn nullable(&self) -> bool {
        match self {
            Regex::Empty => false,
            Regex::Eps => true,
            Regex::Char(_) => false,
            Regex::Or(e1, e2) => e1.nullable() || e2.nullable(),
            Regex::And(e1, e2) => e1.nullable() && e2.nullable(),
            Regex::Seq(e1, e2) => e1.nullable() && e2.nullable(),
            Regex::Star(_) => true,
            Regex::Neg(e) => !e.nullable(),
        }
    }
    pub fn deriv(&self, c: C) -> Self {
        match self {
            Regex::Empty => Regex::Empty,
            Regex::Eps => Regex::Empty,
            Regex::Char(c2) if &c == c2 => Regex::Eps,
            Regex::Char(_) => Regex::Empty,
            Regex::Or(e1, e2) => or(e1.deriv(c), e2.deriv(c)),
            Regex::And(e1, e2) => and(e1.deriv(c), e2.deriv(c)),
            Regex::Seq(e1, e2) if e1.nullable() => {
                or(seq(e1.deriv(c), e2.as_ref().clone()), e2.deriv(c))
            }
            Regex::Seq(e1, e2) => seq(e1.deriv(c), e2.as_ref().clone()),
            Regex::Star(e) => seq(e.deriv(c), star(e.as_ref().clone())),
            Regex::Neg(e) => neg(e.deriv(c)),
        }
    }
    pub fn accepts(&self, cs: impl IntoIterator<Item = C>) -> bool {
        let mut r = self.clone();
        for c in cs {
            r = r.deriv(c);
        }
        r.nullable()
    }
    pub fn canonical(&self) -> Regex<C> {
        // eprintln!("–––––––––––––––––––––––");
        // eprintln!("> {self:?}");
        let res = match self {
            Regex::Empty => empty,
            Regex::Eps => eps,
            Regex::Char(c) => seq(char(*c), eps),
            Regex::Or(e1, e2) => match (e1.canonical(), e2.canonical()) {
                (Regex::Empty, e2) => e2,
                (e1, Regex::Empty) => e1,
                (e1, e2) => seq(or(e1.canonical(), e2.canonical()), eps),
            },
            Regex::And(_e1, _e2) => panic!("Cannot canonicalize regex {self:?}"),
            Regex::Seq(e1, e2) => match (e1.canonical(), e2.canonical()) {
                (Regex::Empty, _e2) => empty,
                (_e1, Regex::Empty) => empty,
                (Regex::Eps, e2) => e2,
                (e1, Regex::Eps) => e1,
                (Regex::Seq(e1, e2), e3) => seq(*e1, seq(*e2, e3).canonical()),
                (e1, e2) => seq(e1, e2),
            },
            Regex::Star(e) => match e.canonical() {
                Regex::Empty => eps,
                e => seq(star(e), eps),
            },
            Regex::Neg(_) => panic!("Cannot canonicalize regex {self:?}"),
        };
        // eprintln!("< {res:?}");
        res
    }
    fn is_canonical_(&self) -> bool {
        match self {
            Regex::Empty => false,
            Regex::Eps => true,
            Regex::Char(_) => false,
            Regex::Or(_, _) => false,
            Regex::And(_, _) => panic!("Cannot check canonicalize regex {self:?}"),
            Regex::Seq(e1, e2) => match e1.as_ref() {
                Regex::Empty => false,
                Regex::Eps => false,
                Regex::Char(_) => e2.is_canonical_(),
                Regex::Or(e11, e12) => {
                    e11.is_canonical_() && e12.is_canonical_() && e2.is_canonical_()
                }
                Regex::And(_, _) => panic!("Cannot check canonicalize regex {self:?}"),
                Regex::Seq(_, _) => false,
                Regex::Star(e11) => e11.is_canonical_() && e2.is_canonical_(),
                Regex::Neg(_) => panic!("Cannot check canonicalize regex {self:?}"),
            },
            Regex::Star(_) => false,
            Regex::Neg(_) => panic!("Cannot check canonicalize regex {self:?}"),
        }
    }
    pub fn is_canonical(&self) -> bool {
        match self {
            Regex::Empty => true,
            _ => self.is_canonical_(),
        }
    }
    pub fn simplify(&self) -> Regex<C> {
        match self {
            Regex::Empty => empty,
            Regex::Eps => eps,
            Regex::Char(c) => char(*c),
            Regex::Or(e1, e2) => match (e1.simplify(), e2.simplify()) {
                (Regex::Empty, e2) => e2,
                (e1, Regex::Empty) => e1,
                (e1, e2) => or(e1, e2),
            },
            Regex::And(e1, e2) => match (e1.simplify(), e2.simplify()) {
                (Regex::Neg(e1), e2) if *e1 == empty => e2,
                (e1, Regex::Neg(e2)) if *e2 == empty => e1,
                (Regex::Empty, _e2) => empty,
                (_e1, Regex::Empty) => empty,
                (Regex::Eps, _) => eps,
                (_, Regex::Eps) => eps,
                (e1, e2) => and(e1, e2),
            },
            Regex::Seq(e1, e2) => match (e1.simplify(), e2.simplify()) {
                (Regex::Empty, _e2) => empty,
                (_e1, Regex::Empty) => empty,
                (Regex::Eps, e2) => e2,
                (e1, Regex::Eps) => e1,
                (e1, e2) => seq(e1, e2),
            },
            Regex::Star(e) => match e.simplify() {
                Regex::Empty => eps,
                Regex::Eps => eps,
                Regex::Star(e) => star(e.as_ref().clone()),
                e => star(e),
            },
            Regex::Neg(e) => match e.simplify() {
                Regex::Neg(e) => *e,
                e => neg(e),
            },
        }
    }
    // fn deriv_re_(&self, e: &Regex<C>) -> Regex<C> {
    //     match self {
    //         Regex::Empty => neg(empty),
    //         Regex::Eps => e.clone(),
    //         Regex::Seq(e1, e2) => match e1.as_ref() {
    //             Regex::Char(c) => e2.deriv_re_(&e.deriv(*c)),
    //             Regex::Or(e11, e12) => and(
    //                 seq(e11.as_ref().clone(), e2.as_ref().clone()).deriv_re_(e),
    //                 seq(e12.as_ref().clone(), e2.as_ref().clone()).deriv_re_(e),
    //             ),
    //             // FIXME: this case is non-terminating...
    //             Regex::Star(e11) => and(
    //                 seq(
    //                     e11.as_ref().clone(),
    //                     seq(star(e11.as_ref().clone()), e2.as_ref().clone()),
    //                 )
    //                 .deriv_re_(e),
    //                 e2.deriv_re_(e),
    //             ),
    //             Regex::Empty => unreachable!(),
    //             Regex::Eps => unreachable!(),
    //             Regex::And(_, _) => unreachable!(),
    //             Regex::Seq(_, _) => unreachable!(),
    //             Regex::Neg(_) => unreachable!(),
    //         },
    //         Regex::Char(_) => unreachable!(),
    //         Regex::Or(_, _) => unreachable!(),
    //         Regex::And(_, _) => unreachable!(),
    //         Regex::Star(_) => unreachable!(),
    //         Regex::Neg(_) => unreachable!(),
    //     }
    // }
    pub fn deriv_re(&self, e: &Regex<C>) -> Regex<C> {
        let c = self.canonical();
        assert!(
            c.is_canonical(),
            "{c:?} is not canonical. Original: {self:?}"
        );
        // c.deriv_re_(e)
        let x = derive_re::gfp(HashSet::new(), e, &c);
        // eprintln!("–––––––––––––––––––––––––––––––");
        // for (r, s) in &x {
        //     eprintln!("{r:?}");
        //     eprintln!("{s:?}");
        //     eprintln!("");
        // }
        // eprintln!("–––––––––––––––––––––––––––––––");
        let y = derive_re::gamma_set(&x);
        y
    }
}

mod derive_re {
    use super::*;
    use std::{collections::HashSet, hash::Hash};

    pub fn gfp<C: Copy + Debug + Eq + Hash>(
        mut x: HashSet<(Regex<C>, Regex<C>)>,
        r: &Regex<C>,
        s: &Regex<C>,
    ) -> HashSet<(Regex<C>, Regex<C>)> {
        let r = &r.canonical();
        let s_simp = s.simplify();
        for (r1, s1) in &x {
            if r == r1 && &s_simp == s1 {
                return x;
            }
        }
        x.insert((r.clone(), s.simplify()));
        match r {
            Regex::Empty => x,
            Regex::Eps => x,
            Regex::Seq(r1, r2) => match r1.as_ref() {
                Regex::Char(c) => gfp(x, r2, &s.deriv(*c)),
                Regex::Or(r11, r12) => {
                    let x = gfp(x, &seq(r11.as_ref().clone(), r2.as_ref().clone()), s);
                    let x = gfp(x, &seq(r12.as_ref().clone(), r2.as_ref().clone()), s);
                    x
                }
                Regex::Star(r11) => {
                    let x = gfp(x, r2.as_ref(), s);
                    let x = gfp(
                        x,
                        &seq(
                            r11.as_ref().clone(),
                            seq(star(r11.as_ref().clone()), r2.as_ref().clone()),
                        ),
                        s,
                    );
                    x
                }
                _ => unreachable!(),
            },
            Regex::Char(_) => unreachable!(),
            Regex::Or(_, _) => unreachable!(),
            Regex::And(_, _) => unreachable!(),
            Regex::Star(_) => unreachable!(),
            Regex::Neg(_) => unreachable!(),
        }
    }

    pub fn gamma<C: Copy>(r: &Regex<C>, s: &Regex<C>) -> Regex<C> {
        match r {
            Regex::Eps => s.clone(),
            _ => neg(empty),
        }
    }

    pub fn gamma_set<C: Copy + Hash>(x: &HashSet<(Regex<C>, Regex<C>)>) -> Regex<C> {
        let mut e = neg(empty);
        for (r, s) in x {
            e = and(e, gamma(r, s))
        }
        e
    }
}

#[derive(Clone, Debug)]
pub struct DFA<C> {
    pub states: HashMap<usize, HashMap<C, usize>>,
    pub init: usize,
    pub finals: HashSet<usize>,
}

impl<C: Display + Hash + Ord> Display for DFA<C> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "init: {}\n", self.init)?;
        write!(f, "finals: {:?}\n", self.finals)?;
        write!(f, "states:\n")?;
        let mut states = self.states.iter().collect::<Vec<_>>();
        states.sort_by_key(|(k, _)| **k);
        for (s, tgts) in states {
            write!(f, "  {s}:\n")?;
            for (p, t) in tgts {
                write!(f, "    {t}: {p}\n")?;
            }
        }
        Ok(())
    }
}

impl<C: Copy + Debug + Eq + Hash + Display + Example + Realizable> Regex<C> {
    pub fn to_dfa(&self) -> DFA<C> {
        let init = 0;
        let mut finals = HashSet::new();
        let mut states = HashMap::new();
        states.insert(init, HashMap::new());
        let mut cache = HashMap::new();
        let e = self.simplify();
        cache.insert(e.clone(), init);
        e.to_dfa_(init, &mut states, &mut finals, &mut cache);
        DFA {
            init,
            finals,
            states,
        }
    }
    pub fn to_dfa_(
        &self,
        cur_state: usize,
        states: &mut HashMap<usize, HashMap<C, usize>>,
        finals: &mut HashSet<usize>,
        cache: &mut HashMap<Regex<C>, usize>,
    ) {
        // println!("Converting {}", self);
        if self.nullable() {
            finals.insert(cur_state);
        }
        for p in self.partitions() {
            if let Some(c) = p.example() {
                let e = self.deriv(c).simplify();
                let mut is_new = false;
                let tgt_state = *cache.entry(e.clone()).or_insert_with(|| {
                    let i = states.len();
                    states.insert(i, HashMap::new());
                    is_new = true;
                    i
                });
                for c in p.realize() {
                    states.get_mut(&cur_state).unwrap().insert(c, tgt_state);
                }
                if is_new {
                    e.to_dfa_(tgt_state, states, finals, cache);
                }
            }
        }
    }

    pub fn partitions(&self) -> Vec<Pattern<C>> {
        match self {
            Regex::Empty => vec![Pattern::everything()],
            Regex::Eps => vec![Pattern::everything()],
            Regex::Char(c) => vec![
                Pattern::positive_char(c.clone()),
                Pattern::negative_char(c.clone()),
            ],
            Regex::Or(e1, e2) => pattern::cartesian_intersection(e1.partitions(), e2.partitions()),
            Regex::And(e1, e2) => pattern::cartesian_intersection(e1.partitions(), e2.partitions()),
            Regex::Seq(e1, e2) if e1.nullable() => {
                pattern::cartesian_intersection(e1.partitions(), e2.partitions())
            }
            Regex::Seq(e1, _) => e1.partitions(),
            Regex::Star(e) => e.partitions(),
            Regex::Neg(e) => e.partitions(),
        }
    }
}

pub struct GNFA<C> {
    pub init: usize,
    pub final_: usize,
    pub states: HashMap<usize, HashMap<usize, Regex<C>>>,
}

impl<C: Copy + Debug + Eq + Hash + Display + Example> Pattern<C> {
    pub fn to_regex(&self) -> Regex<C> {
        let mut r = empty;
        for c in &self.chars {
            r = or(r, char(*c));
        }
        if !self.positive {
            r = neg(r);
        }
        r
    }
}

impl<C: Copy + Debug + Eq + Hash + Display + Example + Realizable> DFA<C> {
    pub fn next_free_state(&self) -> usize {
        self.states.keys().max().map(|i| *i + 1).unwrap_or(0)
    }
    pub fn to_gnfa(&self) -> GNFA<C> {
        let init = self.next_free_state();
        let final_ = init + 1;
        let mut states = HashMap::new();
        for (s, tgts) in &self.states {
            let mut tgts_out = HashMap::new();
            if self.finals.contains(s) {
                tgts_out.insert(final_, eps);
            }
            for (c, tgt) in tgts {
                tgts_out.insert(*tgt, char(*c));
            }
            states.insert(*s, tgts_out);
        }

        states.insert(final_, HashMap::new());

        let mut init_tgts = HashMap::new();
        init_tgts.insert(self.init, eps);
        states.insert(init, init_tgts);

        GNFA {
            init,
            final_,
            states,
        }
    }
    pub fn to_regex(&self) -> Regex<C> {
        self.to_gnfa().to_regex()
    }
    pub fn remove_unreachable_states(&mut self) {
        // Find reachable states
        let mut reachable = HashSet::new();
        let mut queue = VecDeque::new();
        queue.push_back(self.init);
        while let Some(s) = queue.pop_front() {
            reachable.insert(s);
            let tgts = self.states.get_mut(&s).unwrap();
            for (_c, t) in tgts.iter() {
                queue.push_back(*t);
            }
        }

        // Remove unreachable states
        let keys = self.states.keys().cloned().collect::<Vec<_>>();
        for s in keys {
            if !reachable.contains(&s) {
                self.states.remove(&s);
            }
        }
    }
    pub fn remove_dead_states(&mut self) {
        // Find alive states
        let mut alive = HashSet::new();
        let mut queue = VecDeque::new();
        queue.extend(self.finals.iter().cloned());
        while let Some(s) = queue.pop_front() {
            alive.insert(s);
            for (src, _) in self.sources(s) {
                queue.push_back(src);
            }
        }

        // Remove dead states
        let keys = self.states.keys().cloned().collect::<Vec<_>>();
        for s in keys {
            if !alive.contains(&s) {
                self.states.remove(&s);
            }
        }

        // Remove edges to dead
        for tgts in self.states.values_mut() {
            let dead = tgts
                .iter()
                .filter_map(|(c, tgt)| if alive.contains(tgt) { None } else { Some(*c) })
                .collect::<HashSet<_>>();
            for d in dead {
                tgts.remove(&d);
            }
        }
    }
    pub fn sources(&self, tgt: usize) -> HashMap<usize, C> {
        let mut srcs = HashMap::new();
        for (src, tgts) in &self.states {
            if let Some((c, _)) = tgts.iter().find(|(_, tgt2)| tgt == **tgt2) {
                srcs.insert(*src, *c);
            }
        }
        srcs
    }
    pub fn merge_duplicate_states(&mut self) {
        // Build partitions of equivalent states
        let finals = self.finals.iter().cloned().collect::<BTreeSet<_>>();
        let states = self.states.keys().cloned().collect::<BTreeSet<_>>();
        let non_finals = states.difference(&finals).cloned().collect::<BTreeSet<_>>();

        let mut partitions = HashSet::from([finals.clone(), non_finals.clone()]);
        let mut queue = VecDeque::from([finals.clone(), non_finals.clone()]);

        while let Some(a) = queue.pop_front() {
            let mut a_srcs = HashSet::new();
            for s in a {
                a_srcs.extend(self.sources(s));
            }

            for c in Pattern::everything().realize() {
                let x = a_srcs
                    .iter()
                    .cloned()
                    .filter_map(|(s, c2)| if c2 == c { Some(s) } else { None })
                    .collect::<BTreeSet<_>>();
                let mut partitions2 = HashSet::new();
                for y in partitions {
                    let xy = x.intersection(&y).cloned().collect::<BTreeSet<_>>();
                    let yx = y.difference(&x).cloned().collect::<BTreeSet<_>>();
                    if !xy.is_empty() && !yx.is_empty() {
                        partitions2.insert(xy.clone());
                        partitions2.insert(yx.clone());
                        if let Some(i) = queue.iter().position(|x| x == &y) {
                            queue.remove(i);
                            queue.push_back(xy);
                            queue.push_back(yx);
                        } else {
                            if xy.len() <= yx.len() {
                                queue.push_back(xy);
                            } else {
                                queue.push_back(yx);
                            }
                        }
                    } else {
                        partitions2.insert(y);
                    }
                }
                partitions = partitions2;
            }
        }

        // Replace all equivalent state with a single representing state.
        for p in partitions {
            if p.len() > 1 {
                let mut it = p.into_iter();
                let winner = it.next().unwrap();
                let loosers = it.collect::<HashSet<_>>();
                for looser in &loosers {
                    self.states.remove(looser);
                }
                for tgts in self.states.values_mut() {
                    for tgt in tgts.values_mut() {
                        if loosers.contains(tgt) {
                            *tgt = winner;
                        }
                    }
                }
            }
        }
    }
}

impl<C: Copy + Debug + Eq + Hash + Display + Example> GNFA<C> {
    pub fn remove_sources(&mut self, s: usize) -> HashMap<usize, Regex<C>> {
        let mut srcs = HashMap::new();
        for (src, tgts) in &mut self.states {
            if let Some(r) = tgts.remove(&s) {
                srcs.insert(*src, r);
            }
        }
        srcs
    }
    pub fn to_regex(self) -> Regex<C> {
        let mut a = self;
        while a.states.len() > 2 {
            let s = *a
                .states
                .keys()
                .find(|k| **k != a.init && **k != a.final_)
                .unwrap();
            let mut tgts = a.states.remove(&s).unwrap();
            let srcs = a.remove_sources(s);
            let loop_r = tgts.remove(&s).map(star);
            for (src, src_r) in &srcs {
                for (tgt, tgt_r) in &tgts {
                    let r = if let Some(loop_r) = &loop_r {
                        seq(src_r.clone(), seq(loop_r.clone(), tgt_r.clone()))
                    } else {
                        seq(src_r.clone(), tgt_r.clone())
                    };
                    // a.states.get_mut(src).unwrap().insert(*tgt, r);
                    let r2 = r.clone();
                    a.states
                        .get_mut(src)
                        .unwrap()
                        .entry(*tgt)
                        .and_modify(|v| *v = or(v.clone(), r))
                        .or_insert(r2);
                }
            }
        }
        a.states[&a.init].iter().next().unwrap().1.clone()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
        // eprintln!("{}", e1.to_dfa());
        assert_eq!(e1, e1.to_dfa().to_regex().simplify());
    }
}
