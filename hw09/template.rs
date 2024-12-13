use std::collections::{HashMap, HashSet};

pub type Var = String;
pub type Loc = usize;
pub type Store = HashMap<Loc, Term>;
pub type Env = HashMap<Var, Type>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Unit,
    Fun { from: Box<Type>, to: Box<Type> },
    Ref(Box<Type>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Term {
    Var(Var),
    Lambda {
        var: Var,
        annot: Type,
        body: Box<Term>,
    },
    App {
        fun: Box<Term>,
        arg: Box<Term>,
    },
    Unit,
    Ref(Box<Term>),
    Deref(Box<Term>),
    Assign {
        to: Box<Term>,
        body: Box<Term>,
    },
    Loc(Loc),
}

impl Term {
    pub fn seq(t1: Term, t2: Term) -> Term {
        let mut vars = t1.free_vars();
        vars.extend(t2.free_vars());
        let new_v = fresh_var(&vars);
        Term::App {
            fun: Box::new(Term::Lambda {
                var: new_v,
                annot: Type::Unit,
                body: Box::new(t2),
            }),
            arg: Box::new(t1),
        }
    }

    pub fn is_value(&self) -> bool {
        todo!()
    }

    pub fn free_vars(&self) -> HashSet<Var> {
        todo!()
    }

    pub fn subst(self, v: &Var, t: Term) -> Term {
        match self {
            Term::Var(var) => {
                if *v == var {
                    t
                } else {
                    Term::Var(var)
                }
            }
            Term::Lambda { var, annot, body } => {
                let mut vars = body.free_vars();
                vars.insert(var.clone());
                let fresh_v = fresh_var(&vars);
                let body_subst = (*body).subst(&var, Term::Var(fresh_v.clone()));
                Term::Lambda {
                    var: fresh_v,
                    annot,
                    body: Box::new(body_subst.subst(v, t)),
                }
            }
            Term::App { fun, arg } => Term::App {
                fun: Box::new((*fun).subst(v, t.clone())),
                arg: Box::new((*arg).subst(v, t)),
            },
            Term::Unit => Term::Unit,
            Term::Ref(term) => Term::Ref(Box::new((*term).subst(v, t))),
            Term::Deref(term) => Term::Deref(Box::new((*term).subst(v, t))),
            Term::Assign { to, body } => Term::Assign {
                to: Box::new((*to).subst(v, t.clone())),
                body: Box::new((*body).subst(v, t)),
            },
            Term::Loc(loc) => Term::Loc(loc),
        }
    }
}

pub fn fresh_var(used_vars: &HashSet<Var>) -> Var {
    let mut i = 0;
    while used_vars.contains(&format!("x{i}")) {
        i += 1;
    }
    format!("x{i}")
}

pub fn fresh_loc(st: &Store) -> Loc {
    let mut next_loc = 0;
    while st.contains_key(&next_loc) {
        next_loc += 1;
    }
    next_loc
}

pub fn eval(t: Term, st: &mut Store) -> Result<Term, String> {
    let evaled = eval_once(t.clone(), st)?;
    if evaled == t {
        Ok(evaled)
    } else {
        eval(evaled, st)
    }
}

pub fn eval_once(t: Term, st: &mut Store) -> Result<Term, String> {
    todo!()
}

pub fn check(t: Term, env: &mut Env) -> Result<Type, String> {
    todo!()
}

fn main() {
    let example1 = Term::App {
        fun: Box::new(Term::Lambda {
            var: "x".to_owned(),
            annot: Type::Ref(Box::new(Type::Unit)),
            body: Box::new(Term::Deref(Box::new(Term::Var("x".to_owned())))),
        }),
        arg: Box::new(Term::App {
            fun: Box::new(Term::Lambda {
                var: "y".to_owned(),
                annot: Type::Unit,
                body: Box::new(Term::Ref(Box::new(Term::Var("y".to_owned())))),
            }),
            arg: Box::new(Term::Unit),
        }),
    };

    let example2 = Term::App {
        fun: Box::new(Term::Lambda {
            var: "x".to_owned(),
            annot: Type::Ref(Box::new(Type::Unit)),
            body: Box::new(Term::Assign {
                to: Box::new(Term::Var("x".to_owned())),
                body: Box::new(Term::Deref(Box::new(Term::Var("x".to_owned())))),
            }),
        }),
        arg: Box::new(Term::Ref(Box::new(Term::Unit))),
    };

    let example1_checked = check(example1.clone(), &mut HashMap::new());
    let example2_checked = check(example2.clone(), &mut HashMap::new());
    let example1_evaled = eval(example1, &mut HashMap::new());
    let example2_evaled = eval(example2, &mut HashMap::new());
    assert_eq!(example1_checked, Ok(Type::Unit));
    assert_eq!(example2_checked, Ok(Type::Unit));
    assert_eq!(example1_evaled, Ok(Term::Unit));
    assert_eq!(example2_evaled, Ok(Term::Unit));
}
