use std::collections::HashMap;

use crate::{
    syntax::{Expr, Id, Loc, SExpr, SId, SRegex},
    util::pretty::{Assoc, Pretty},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    Abs(Env, SId, SExpr),
    Unit,
    Pair(Box<Value>, Box<Value>),
    Loc(Loc),
}

impl Pretty<()> for Env {
    fn pp(&self, p: &mut crate::util::pretty::PrettyEnv<()>) {
        let mut env = self.env.iter().collect::<Vec<_>>();
        env.sort_by_key(|(x, _v)| *x);
        for (i, (x, v)) in env.into_iter().enumerate() {
            if i != 0 {
                p.pp(", ");
            }
            p.pp(x);
            p.pp(" ↦ ");
            p.pp(v);
        }
    }
}

impl Pretty<()> for HeapVal {
    fn pp(&self, p: &mut crate::util::pretty::PrettyEnv<()>) {
        p.pp("Resource: ");
        p.pp(&self.regex);
        p.pp(", Output: ");
        p.pp(&self.output);
        p.pp(", Ref-Count: ");
        p.pp(&self.ref_count.to_string());
        p.pp(", Valid Output: ");
        p.pp(&self
            .regex
            .accepts(self.output.as_bytes().into_iter().cloned())
            .to_string());
    }
}

impl Pretty<()> for Heap {
    fn pp(&self, p: &mut crate::util::pretty::PrettyEnv<()>) {
        let mut env = self.heap.iter().collect::<Vec<_>>();
        env.sort_by_key(|(x, _v)| *x);
        for (i, (x, v)) in env.into_iter().enumerate() {
            if i != 0 {
                p.pp(", ");
            }
            p.pp(&x.to_string());
            p.pp(" ↦ ");
            p.pp(v);
        }
    }
}

impl Pretty<()> for Value {
    fn pp(&self, p: &mut crate::util::pretty::PrettyEnv<()>) {
        use Assoc::None as N;
        use Assoc::Right as R;
        match self {
            Value::Abs(env, x, e) => p.infix(1, R, |p| {
                p.pp("λ[");
                p.pp(env);
                p.pp("] ");
                p.pp(x);
                p.pp(". ");
                p.pp_arg(R, e);
                p.pp("");
            }),
            Value::Unit => p.pp("unit"),
            Value::Pair(v1, v2) => p.infix(0, N, |p| {
                p.pp(v1);
                p.pp(", ");
                p.pp(v2);
            }),
            Value::Loc(l) => p.pp(&l.to_string()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HeapVal {
    regex: SRegex,
    ref_count: usize,
    output: String,
}

pub struct Heap {
    pub heap: HashMap<Loc, HeapVal>,
    pub next_loc: usize,
}

impl Heap {
    pub fn new() -> Self {
        Self {
            heap: HashMap::new(),
            next_loc: 0,
        }
    }
    pub fn insert(&mut self, regex: SRegex) -> Loc {
        let loc = self.next_loc;
        self.next_loc += 1;
        self.heap.insert(
            loc,
            HeapVal {
                regex,
                ref_count: 1,
                output: String::new(),
            },
        );
        loc
    }
    pub fn get_mut_from(&mut self, loc: Loc, src: &SExpr) -> Result<&mut HeapVal, EvalError> {
        self.heap
            .get_mut(&loc)
            .ok_or_else(|| EvalError::UndefinedLoc(src.clone(), loc))
    }
    pub fn get_mut_from_val(
        &mut self,
        v: &Value,
        src: &SExpr,
    ) -> Result<(Loc, &mut HeapVal), EvalError> {
        match v {
            Value::Loc(l) => Ok((*l, self.get_mut_from(*l, src)?)),
            _ => Err(EvalError::ValMismatch(
                src.clone(),
                format!("resource location"),
                v.clone(),
            )),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Env {
    pub env: HashMap<Id, Value>,
}

impl Env {
    pub fn new() -> Self {
        Self {
            env: HashMap::new(),
        }
    }
    pub fn ext(&self, x: Id, v: Value) -> Env {
        let mut env = self.clone();
        env.env.insert(x, v);
        env
    }
    pub fn get(&self, x: &SId) -> Result<Value, EvalError> {
        self.env
            .get(&x.val)
            .cloned()
            .ok_or_else(|| EvalError::UndefinedVar(x.clone()))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EvalError {
    ValMismatch(SExpr, String, Value),
    UndefinedLoc(SExpr, usize),
    ClosedUnfinished(SExpr, SRegex, String),
    UndefinedVar(SId),
}

pub fn eval_(heap: &mut Heap, env: &Env, e: &SExpr) -> Result<Value, EvalError> {
    match &e.val {
        Expr::Unit => Ok(Value::Unit),
        Expr::New(r) => {
            let loc = heap.insert(r.clone());
            Ok(Value::Loc(loc))
        }
        Expr::Write(w, e1) => {
            let v1 = eval_(heap, env, e1)?;
            let (l, vl) = heap.get_mut_from_val(&v1, e1.as_ref())?;
            vl.output += &w;
            Ok(Value::Loc(l))
        }
        Expr::Split(_r, e1) => {
            let v1 = eval_(heap, env, e1)?;
            let (l, vl) = heap.get_mut_from_val(&v1, e1.as_ref())?;
            vl.ref_count += 1;
            Ok(Value::Pair(
                Box::new(Value::Loc(l)),
                Box::new(Value::Loc(l)),
            ))
        }
        Expr::Close(e1) => {
            let v1 = eval_(heap, env, e1)?;
            let (l, vl) = heap.get_mut_from_val(&v1, e1.as_ref())?;
            if vl.ref_count > 1 {
                vl.ref_count -= 1;
            } else {
                if vl.regex.accepts(vl.output.as_bytes().into_iter().cloned()) {
                    heap.heap.remove(&l);
                } else {
                    Err(EvalError::ClosedUnfinished(
                        *e1.clone(),
                        vl.regex.clone(),
                        vl.output.clone(),
                    ))?
                }
            }
            Ok(Value::Unit)
        }
        Expr::Loc(l) => Ok(Value::Loc(l.val)),
        Expr::Var(x) => env.get(x),
        Expr::Abs(_om, x, e) => Ok(Value::Abs(env.clone(), x.clone(), *e.clone())),
        Expr::App(e1, e2) => {
            // FIXME: Multiplicity required for evaluation order.
            let v1 = eval_(heap, env, e1)?;
            let v2 = eval_(heap, env, e2)?;
            match v1 {
                Value::Abs(env, x, e) => {
                    let env = env.ext(x.val, v2);
                    eval_(heap, &env, &e)
                }
                _ => Err(EvalError::ValMismatch(
                    e.clone(),
                    format!("function"),
                    v1.clone(),
                )),
            }
        }
        Expr::AppBorrow(_, _) => panic!("Borrows are not implemented, yet."),
        Expr::Pair(_om, e1, e2) => {
            let v1 = eval_(heap, env, e1)?;
            let v2 = eval_(heap, env, e2)?;
            Ok(Value::Pair(Box::new(v1), Box::new(v2)))
        }
        Expr::LetPair(x, y, e1, e2) => {
            let v1 = eval_(heap, env, e1)?;
            match v1 {
                Value::Pair(vx, vy) => {
                    let env = env.ext(x.val.clone(), *vx).ext(y.val.clone(), *vy);
                    eval_(heap, &env, &e2)
                }
                _ => Err(EvalError::ValMismatch(
                    *e1.clone(),
                    format!("pair"),
                    v1.clone(),
                )),
            }
        }
        Expr::Let(x, e1, e2) => {
            let vx = eval_(heap, env, e1)?;
            let env = env.ext(x.val.clone(), vx);
            eval_(heap, &env, &e2)
        }
        Expr::Seq(e1, e2) => {
            let _ = eval_(heap, env, e1)?;
            eval_(heap, env, &e2)
        }
        Expr::Ann(e, _t) => eval_(heap, env, e.as_ref()),
    }
}

pub fn eval(e: &SExpr) -> Result<(Heap, Value), EvalError> {
    let mut heap = Heap::new();
    let env = Env::new();
    let v = eval_(&mut heap, &env, e)?;
    Ok((heap, v))
}
