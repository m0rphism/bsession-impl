use std::collections::{HashMap, HashSet, VecDeque};
use std::hash::Hash;

use crate::syntax::{SId, SType, Type};
use crate::typechecker::is_unr;
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

impl Ctx {
    pub fn vars(&self) -> HashSet<Id> {
        let mut res = HashSet::new();
        self.map_binds(&mut |x, _t| {
            res.insert(x.clone());
        });
        res
    }

    pub fn is_splittable(&self, xs: &HashSet<Id>) -> bool {
        match self {
            Ctx::Empty => true,
            Ctx::Bind(_x, _t) => true,
            Ctx::Join(c1, c2, o) => c1.is_splittable(xs) && c2.is_splittable(xs),
        }
    }
    pub fn to_sem(&self) -> SemCtx {
        match self {
            Ctx::Empty => SemCtx::empty(),
            Ctx::Bind(x, t) => SemCtx::bind(x.val.clone(), t.val.clone()),
            Ctx::Join(c1, c2, o) => c1.to_sem().join(&c2.to_sem(), *o),
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

/// INVARIANT: each label corresponds to exactly one node.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Graph<L> {
    pub edges: HashMap<usize, (L, HashSet<usize>)>,
}

impl<L: Clone + Eq + Hash> Graph<L> {
    pub fn empty() -> Self {
        Graph {
            edges: HashMap::new(),
        }
    }
    pub fn singleton(l: L) -> Self {
        let mut g = Graph::empty();
        g.edges.insert(0, (l, HashSet::new()));
        g
    }
    pub fn next_free_node(&self) -> usize {
        self.edges.keys().max().cloned().unwrap_or(0)
    }
    pub fn union(&self, other: &Graph<L>) -> Self {
        let mut out = self.clone();
        let offset = self.next_free_node();
        for (src, (l, tgts)) in &other.edges {
            let new_src = src + offset;
            let new_targets = tgts.iter().map(|tgt| *tgt + offset).collect();
            out.edges.insert(new_src, (l.clone(), new_targets));
        }
        out
    }
    pub fn plus(&self, other: &Graph<L>) -> Self {
        let mut out = self.union(other);
        let offset = self.next_free_node();
        for src in self.edges.keys() {
            for tgt in other.edges.keys() {
                let new_tgt = tgt + offset;
                out.edges.get_mut(&src).unwrap().1.insert(new_tgt);
            }
        }
        out
    }
    pub fn labels(&self) -> impl Iterator<Item = &L> {
        self.edges.values().map(|(l, _)| l)
    }
    pub fn label_map(&self) -> HashMap<&L, (usize, &HashSet<usize>)> {
        self.edges
            .iter()
            .map(|(src, (l, tgts))| (l, (*src, tgts)))
            .collect()
    }
    pub fn is_subgraph_of(&self, other: &Self) -> bool {
        // let labels1 = self.labels().collect::<HashSet<_>>();
        // let labels2 = other.labels().collect::<HashSet<_>>();
        // if labels1 != labels2 {
        //     return false;
        // }

        let labels1 = self.label_map();
        let labels2 = other.label_map();

        let mut queue = VecDeque::new();
        for (src, (l, tgts)) in &self.edges {
            if let Some((src2, tgts2)) = labels2.get(l) {
                queue.push_back((src, src2))
            } else {
                return false;
            }
        }

        while let Some((src1, src2)) = queue.pop_front() {
            let (l1, tgts1) = &self.edges[src1];
            let (l2, tgts2) = &other.edges[src2];
            for tgt1 in tgts1 {}
        }

        true
    }
    // pub fn reachable_fn(mut src: impl FnMut(usize) -> bool, mut tgt: impl FnMut(usize) -> bool) -> bool {

    // }
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

    pub fn pull_left(&self) -> Option<(Ctx, JoinOrd)> {
        match self {
            CtxCtx::Hole => Some((Ctx::Empty, JoinOrd::Unordered)),
            CtxCtx::JoinL(cc, c, o) => {
                let (c2, o2) = self.pull_left()?;
                Some((CtxS::Join(c2, c, *o), o2))
            }
            CtxCtx::JoinR(c, cc, o) => todo!(),
        }
    }
}
