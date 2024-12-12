type Id = String

enum Term {
    case Var(x: Id) // variable
    case Abs(x: Id, body: Term) // lambda abstraction
    case App(fn: Term, arg: Term) // application
    case Unit // constant unit value
}

sealed trait Type
case class ArrrowT(arg: Type, ret: Type) extends Type // arrow (function) type
case class UnitT() extends Type // Unit type

// Get a fresh UnificationVar using `UnificationVar.fresh`.
case class UnificationVar(index: Int) extends Type
object UnificationVar {
    private var _count = 0

    /** Creates a fresh [[UnificationVar]]. */
    def fresh(): UnificationVar = {
        _count += 1
        UnificationVar(_count)
    }
}

case class Constraint(l: Type, r: Type)

type Environment = Map[Id, Type]

def typecheck(env: Environment, term: Term): Option[Type] = {
    val (tpe, constraints) = collectConstraints(env, term)
    unify(constraints) match {
    case Some(unifier) => Some(applyUnifier(unifier, tpe))
    case None => None
    }
}

def collectConstraints(env: Environment, term: Term) : (Type, List[Constraint]) = ??? // TODO

type Unifier = Map[UnificationVar, Type]

def unify(constraints: List[Constraint]): Option[Unifier] = ??? // TODO

def occurs(x: UnificationVar, t: Type): Boolean = ??? // TODO

def substitute(x: UnificationVar, by: Type, in: Type): Type = in match {
    case y: UnificationVar if x == y => by
    case ArrrowT(f, a) => ArrrowT(substitute(x, by, f), substitute(x, by, a))
    case other => other
}

def substitute(x: UnificationVar, by: Type, in: List[Constraint]): List[Constraint] = {
    in.map { c => substitute(x, by, c) }
}

def substitute(x: UnificationVar, by: Type, in: Constraint): Constraint = {
    Constraint(substitute(x,by, in.l), substitute(x,by, in.r))
}

def applyUnifier(unifier: Unifier, t: Type): Type = t match {
    case x: UnificationVar if unifier.contains(x) => applyUnifier(unifier, unifier(x))
    case x: UnificationVar => x
    case ArrrowT(arg, ret) => ArrrowT(applyUnifier(unifier, arg), applyUnifier(unifier, ret))
    case other => other
}

@main
def main() = {
    import Term.*
    // prints the result of typechecking (λx. x unit) (λy. y) in the empty context
    print(typecheck(Nil, App(Abs("x", App(Var("x"), Unit)), Abs("y", Var("y")))))
}
