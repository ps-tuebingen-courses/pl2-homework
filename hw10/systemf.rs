use std::collections::HashMap;

pub type Var = String;
pub type TyVar = String;

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
    TyLambda {
        ty: TyVar,
        term: Box<Term>,
    },
    TyApp {
        term: Box<Term>,
        ty: Type,
    },
}

#[derive(Debug, PartialEq, Eq)]
pub enum Type {
    Var(TyVar),
    Fun { from: Box<Type>, to: Box<Type> },
    Forall { var: TyVar, ty: Box<Type> },
}

#[derive(Default)]
pub struct Env {
    vars: HashMap<Var, Type>,
    ty_vars: Vec<TyVar>,
}

pub fn check(t: Term, env: &mut Env) -> Option<Type> {
    todo!()
}

fn main() {
    let id = Term::TyLambda {
        ty: "X".to_owned(),
        term: Box::new(Term::Lambda {
            var: "x".to_owned(),
            annot: Type::Var("X".to_owned()),
            body: Box::new(Term::Var("x".to_owned())),
        }),
    };
    let id_ty = check(id, &mut Default::default()).unwrap();
    assert_eq!(
        id_ty,
        Type::Forall {
            var: "X".to_owned(),
            ty: Box::new(Type::Fun {
                from: Box::new(Type::Var("X".to_owned())),
                to: Box::new(Type::Var("X".to_owned()))
            })
        }
    );

    let double = Term::TyLambda {
        ty: "X".to_owned(),
        term: Box::new(Term::Lambda {
            var: "f".to_owned(),
            annot: Type::Fun {
                from: Box::new(Type::Var("X".to_owned())),
                to: Box::new(Type::Var("X".to_owned())),
            },
            body: Box::new(Term::Lambda {
                var: "a".to_owned(),
                annot: Type::Var("X".to_owned()),
                body: Box::new(Term::App {
                    fun: Box::new(Term::Var("f".to_owned())),
                    arg: Box::new(Term::App {
                        fun: Box::new(Term::Var("f".to_owned())),
                        arg: Box::new(Term::Var("a".to_owned())),
                    }),
                }),
            }),
        }),
    };
    let double_ty = check(double, &mut Default::default()).unwrap();
    assert_eq!(
        double_ty,
        Type::Forall {
            var: "X".to_owned(),
            ty: Box::new(Type::Fun {
                from: Box::new(Type::Fun {
                    from: Box::new(Type::Var("X".to_owned())),
                    to: Box::new(Type::Var("X".to_owned())),
                }),
                to: Box::new(Type::Fun {
                    from: Box::new(Type::Var("X".to_owned())),
                    to: Box::new(Type::Var("X".to_owned())),
                }),
            }),
        }
    );
}
