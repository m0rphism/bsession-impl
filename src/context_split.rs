use std::collections::{HashMap, HashSet, VecDeque};
use std::hash::Hash;

use crate::syntax::{SId, SType, Type};
use crate::typechecker::is_unr;
use crate::util::pretty::{Pretty, PrettyEnv};
use crate::{
    syntax::Id,
    typechecker::{Ctx, CtxCtx, JoinOrd},
};

pub trait Boxed<T> {
    fn boxed(self) -> Box<T>;
}

impl<T> Boxed<T> for T {
    fn boxed(self) -> Box<T> {
        Box::new(self)
    }
}

impl<'a, T: Clone> Boxed<T> for &'a T {
    fn boxed(self) -> Box<T> {
        Box::new(self.clone())
    }
}

impl<T> Boxed<T> for Box<T> {
    fn boxed(self) -> Box<T> {
        self
    }
}

impl<'a, T: Clone> Boxed<T> for &'a Box<T> {
    fn boxed(self) -> Box<T> {
        self.clone()
    }
}

#[allow(non_snake_case)]
pub mod CtxS {
    use super::*;

    #[allow(non_upper_case_globals)]
    pub const Empty: Ctx = Ctx::Empty;

    pub fn Bind(x: SId, t: SType) -> Ctx {
        Ctx::Bind(x, t)
    }

    pub fn Join(c1: impl Boxed<Ctx>, c2: impl Boxed<Ctx>, o: JoinOrd) -> Ctx {
        Ctx::Join(c1.boxed(), c2.boxed(), o)
    }
}

#[allow(non_snake_case)]
pub mod CtxCtxS {
    use super::*;

    #[allow(non_upper_case_globals)]
    pub const Hole: CtxCtx = CtxCtx::Hole;

    pub fn JoinL(cc: impl Boxed<CtxCtx>, c: impl Boxed<Ctx>, o: JoinOrd) -> CtxCtx {
        CtxCtx::JoinL(cc.boxed(), c.boxed(), o)
    }

    pub fn JoinR(c: impl Boxed<Ctx>, cc: impl Boxed<CtxCtx>, o: JoinOrd) -> CtxCtx {
        CtxCtx::JoinR(c.boxed(), cc.boxed(), o)
    }
}

use CtxCtxS::*;
use CtxS::*;
use JoinOrd::*;

impl Ctx {
    pub fn vars(&self) -> HashSet<Id> {
        let mut res = HashSet::new();
        self.map_binds(&mut |x, _t| {
            res.insert(x.clone());
        });
        res
    }
    pub fn binds(&self) -> HashMap<Id, Type> {
        let mut res = HashMap::new();
        self.map_binds(&mut |x, t| {
            res.insert(x.clone(), t.clone());
        });
        res
    }
    pub fn to_sem(&self) -> SemCtx {
        match self {
            Ctx::Empty => SemCtx::empty(),
            Ctx::Bind(x, t) => SemCtx::bind(x.val.clone(), t.val.clone()),
            Ctx::Join(c1, c2, o) => c1.to_sem().join(&c2.to_sem(), *o),
        }
    }
    pub fn is_splittable(&self, xs: &HashSet<Id>) -> bool {
        let sem = self.to_sem();
        let (binds_xs, binds_not_xs) = self
            .binds()
            .into_iter()
            .filter(|(_, t)| !is_unr(t))
            .partition::<HashSet<_>, _>(|(x, _)| xs.contains(x));
        for b1 in &binds_xs {
            for b2 in &binds_not_xs {
                if sem.ord.is_reachable(b1, b2) {
                    for b3 in &binds_xs {
                        if sem.ord.is_reachable(b2, b3) {
                            return false;
                        }
                    }
                }
            }
        }
        true
    }
    pub fn split(&self, xs: &HashSet<Id>) -> Option<Option<(CtxCtx, Ctx)>> {
        match self {
            Ctx::Empty => Some(None), // no splitting necessary
            Ctx::Bind(x, t) if xs.contains(&x.val) => {
                Some(Some((CtxCtxS::Hole, Ctx::Bind(x.clone(), t.clone()))))
            }
            Ctx::Bind(_, _) => Some(None), // no splitting necessary
            Ctx::Join(c1, c2, Ordered) => match (c1.split(xs)?, c2.split(xs)?) {
                (None, None) => Some(None),
                (None, Some((cc, c))) => Some(Some((JoinR(c1, cc, Ordered), c))),
                (Some((cc, c)), None) => Some(Some((JoinL(cc, c2, Ordered), c))),
                (Some((cc1, c1)), Some((cc2, c2))) => {
                    if let (Some(c1x), Some(c2x)) = (cc1.pull_right(), cc2.pull_left()) {
                        Some(Some((
                            JoinR(c1x, JoinL(Hole, c2x, Ordered), Ordered),
                            Join(c1, c2, Ordered),
                        )))
                    } else {
                        None
                    }
                }
            },
            Ctx::Join(c1, c2, Unordered) => match (c1.split(xs)?, c2.split(xs)?) {
                (None, None) => Some(None),
                (None, Some((cc, c))) => Some(Some((JoinR(c1, cc, Unordered), c))),
                (Some((cc, c)), None) => Some(Some((JoinL(cc, c2, Unordered), c))),
                (Some((cc1, c1)), Some((cc2, c2))) => {
                    if let (Some(c1x), Some(c2x)) = (cc1.pull_par(), cc2.pull_par()) {
                        Some(Some((
                            JoinR(c1x, JoinL(CtxCtxS::Hole, c2x, Unordered), Unordered),
                            Join(c1, c2, Unordered),
                        )))
                    } else if let (Some(c1x), Some(c2x)) = (cc1.pull_right(), cc2.pull_right()) {
                        Some(Some((
                            JoinR(Join(c1x, c2x, Unordered), CtxCtxS::Hole, Ordered),
                            Join(c1, c2, Unordered),
                        )))
                    } else if let (Some(c1x), Some(c2x)) = (cc1.pull_left(), cc2.pull_left()) {
                        Some(Some((
                            JoinL(CtxCtxS::Hole, Join(c1x, c2x, Unordered), Ordered),
                            Join(c1, c2, Unordered),
                        )))
                    } else if let (Some(c1x), Some(c2x)) = (cc1.pull_left(), cc2.pull_right()) {
                        Some(Some((
                            JoinR(c2x, JoinL(CtxCtxS::Hole, c1x, Ordered), Ordered),
                            Join(c1, c2, Ordered),
                        )))
                    } else if let (Some(c1x), Some(c2x)) = (cc1.pull_right(), cc2.pull_left()) {
                        Some(Some((
                            JoinR(c1x, JoinL(CtxCtxS::Hole, c2x, Ordered), Ordered),
                            Join(c1, c2, Ordered),
                        )))
                    } else {
                        None
                    }
                }
            },
        }
    }
}

pub struct SemCtx {
    pub ord: Graph<(Id, Type)>,
    pub unr: HashSet<(Id, Type)>,
}

impl SemCtx {
    pub fn empty() -> Self {
        Self {
            ord: Graph::empty(),
            unr: HashSet::new(),
        }
    }
    pub fn bind(x: Id, t: Type) -> Self {
        let mut c = Self::empty();
        if is_unr(&t) {
            c.unr.insert((x, t));
        } else {
            c.ord = Graph::singleton((x, t));
        }
        c
    }
    pub fn join(&self, other: &Self, o: JoinOrd) -> Self {
        match o {
            JoinOrd::Ordered => self.plus(other),
            JoinOrd::Unordered => self.union(other),
        }
    }
    pub fn union(&self, other: &Self) -> Self {
        Self {
            ord: self.ord.union(&other.ord),
            unr: self.unr.union(&other.unr).cloned().collect(),
        }
    }
    pub fn plus(&self, other: &Self) -> Self {
        Self {
            ord: self.ord.plus(&other.ord),
            unr: self.unr.union(&other.unr).cloned().collect(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Graph<L: Eq + Hash> {
    pub edges: HashMap<L, HashSet<L>>,
}

impl<L: Clone + Eq + Hash> Graph<L> {
    pub fn empty() -> Self {
        Graph {
            edges: HashMap::new(),
        }
    }
    pub fn singleton(l: L) -> Self {
        let mut g = Graph::empty();
        g.edges.insert(l, HashSet::new());
        g
    }
    pub fn union(&self, other: &Graph<L>) -> Self {
        let mut out = self.clone();
        for (l, tgts) in &other.edges {
            let e = out.edges.entry(l.clone()).or_insert_with(|| HashSet::new());
            e.extend(tgts.into_iter().cloned());
        }
        out
    }
    pub fn plus(&self, other: &Graph<L>) -> Self {
        let mut out = self.union(other);
        for src in self.edges.keys() {
            for tgt in other.edges.keys() {
                out.edges.get_mut(src).unwrap().insert(tgt.clone());
            }
        }
        out
    }
    pub fn is_subgraph_of(&self, other: &Self) -> bool {
        for (src, tgts) in &self.edges {
            if let Some(tgts2) = &other.edges.get(src) {
                for tgt in tgts {
                    if !tgts2.contains(tgt) {
                        return false;
                    }
                }
            } else {
                return false;
            }
        }
        for src2 in other.edges.keys() {
            if !self.edges.contains_key(src2) {
                return false;
            }
        }
        true
    }
    pub fn is_reachable(&self, src: &L, tgt: &L) -> bool {
        let mut visited = HashSet::<&L>::new();
        let mut queue = VecDeque::new();
        queue.push_back(src);
        while let Some(src) = queue.pop_front() {
            if visited.insert(src) {
                if src == tgt {
                    return true;
                }
                for tgt in &self.edges[src] {
                    queue.push_back(tgt);
                }
            }
        }
        false
    }
}

pub enum PullOut {
    HoleLeft(Ctx),
    HoleRight(Ctx),
    Par(Ctx),
}

impl CtxCtx {
    pub fn is_left(&self) -> bool {
        match self {
            CtxCtx::Hole => true,
            CtxCtx::JoinL(cc1, _c2, _o) => cc1.is_left(),
            CtxCtx::JoinR(c1, cc2, o) => cc2.is_left() && (*o == JoinOrd::Unordered || c1.is_unr()),
        }
    }

    pub fn is_right(&self) -> bool {
        match self {
            CtxCtx::Hole => true,
            CtxCtx::JoinL(cc1, c2, o) => {
                cc1.is_right() && (*o == JoinOrd::Unordered || c2.is_unr())
            }
            CtxCtx::JoinR(_c1, cc2, _o) => cc2.is_right(),
        }
    }

    fn pull_left_(&self) -> Option<Ctx> {
        match self {
            CtxCtx::Hole => Some(Ctx::Empty),
            CtxCtx::JoinL(cc, c, o) => {
                let c2 = cc.pull_left()?;
                Some(CtxS::Join(c2, c, *o))
            }
            CtxCtx::JoinR(c, cc, o) => {
                let c2 = cc.pull_left()?;
                Some(CtxS::Join(c2, c, *o))
            }
        }
    }

    pub fn pull_left(&self) -> Option<Ctx> {
        if self.is_left() {
            self.pull_left_()
        } else {
            None
        }
    }

    fn pull_right_(&self) -> Option<Ctx> {
        match self {
            CtxCtx::Hole => Some(Ctx::Empty),
            CtxCtx::JoinL(cc, c, o) => {
                let c2 = cc.pull_right()?;
                Some(CtxS::Join(c, c2, *o))
            }
            CtxCtx::JoinR(c, cc, o) => {
                let c2 = cc.pull_right()?;
                Some(CtxS::Join(c, c2, *o))
            }
        }
    }

    pub fn pull_right(&self) -> Option<Ctx> {
        if self.is_right() {
            self.pull_right_()
        } else {
            None
        }
    }

    fn pull_par_(&self) -> Option<Ctx> {
        match self {
            CtxCtx::Hole => Some(Ctx::Empty),
            CtxCtx::JoinL(cc, c, _o) => {
                let c2 = cc.pull_par()?;
                Some(CtxS::Join(c, c2, JoinOrd::Unordered))
            }
            CtxCtx::JoinR(c, cc, _o) => {
                let c2 = cc.pull_par()?;
                Some(CtxS::Join(c, c2, JoinOrd::Unordered))
            }
        }
    }

    pub fn pull_par(&self) -> Option<Ctx> {
        if self.is_left() && self.is_right() {
            self.pull_par_()
        } else {
            None
        }
    }
}

impl Pretty<()> for Ctx {
    fn pp(&self, p: &mut PrettyEnv<()>) {
        match self {
            Ctx::Empty => p.pp("·"),
            Ctx::Bind(x, t) => {
                p.pp(x);
                // p.pp(" : ");
                // p.pp(t);
            }
            Ctx::Join(c1, c2, o) => {
                p.pp("(");
                p.pp(c1);
                match o {
                    Ordered => p.pp(" , "),
                    Unordered => p.pp(" ∥ "),
                }
                p.pp(c2);
                p.pp(")")
            }
        }
    }
}

impl Pretty<()> for CtxCtx {
    fn pp(&self, p: &mut PrettyEnv<()>) {
        match self {
            CtxCtx::Hole => p.pp("■"),
            CtxCtx::JoinL(c1, c2, o) => {
                p.pp("(");
                p.pp(c1);
                match o {
                    Ordered => p.pp(" , "),
                    Unordered => p.pp(" ∥ "),
                }
                p.pp(c2);
                p.pp(")")
            }
            CtxCtx::JoinR(c1, c2, o) => {
                p.pp("(");
                p.pp(c1);
                match o {
                    Ordered => p.pp(" , "),
                    Unordered => p.pp(" ∥ "),
                }
                p.pp(c2);
                p.pp(")")
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{typechecker::fake_span, util::pretty::pretty_def};

    use super::*;

    fn bind(x: &str) -> Ctx {
        Bind(fake_span(x.to_string()), fake_span(Type::Unit))
    }

    pub fn u(c1: impl Boxed<Ctx>, c2: impl Boxed<Ctx>) -> Ctx {
        Join(c1, c2, Unordered)
    }
    pub fn o(c1: impl Boxed<Ctx>, c2: impl Boxed<Ctx>) -> Ctx {
        Join(c1, c2, Ordered)
    }

    fn test_split(c: Ctx, xs: impl IntoIterator<Item = &'static str>) {
        let xs: HashSet<Id> = xs.into_iter().map(|x| x.to_string()).collect();
        eprintln!("\n––––––––––––––––––––––––––––––––––––––––––––––––––");
        eprintln!("Ctx:          {}", pretty_def(&c));
        eprintln!("Splittable:   {}", c.is_splittable(&xs));
        match c.split(&xs) {
            Some(Some((cc, c2))) => {
                eprintln!("Split CtxCtx: {}", pretty_def(&cc));
                eprintln!("Split Ctx:    {}", pretty_def(&c2));
            }
            Some(None) => {
                let ys = c
                    .binds()
                    .into_iter()
                    .filter(|(_, t)| is_unr(t))
                    .map(|(x, _)| x)
                    .collect::<HashSet<_>>();
                if ys.is_disjoint(&xs) {
                    eprintln!("No splitting necessary")
                } else {
                    assert!(false, "No splitting done, but variables are not disjoint");
                }
            }
            None => assert!(false, "Failed splitting"),
        }
    }

    #[test]
    fn split_1() {
        test_split(o(bind("x"), bind("y")), ["x"]);
    }
}
