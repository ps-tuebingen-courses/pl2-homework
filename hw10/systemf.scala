import collection.mutable.HashMap;

type Var = String;
type TyVar = String;

enum Term {
  case Variable(v:Var)
  case Lambda(v:Var,annot:Ty,body:Term)
  case App(fun:Term,arg:Term)
  case TyLambda(ty: TyVar, term: Term)
  case TyApp(term: Term, ty: Ty)  
}

enum Ty {
  case VarTy(v:TyVar)
  case FunTy(from: Ty, to: Ty)
  case ForallTy(v:TyVar, ty: Ty)
}

case class Env(vars:HashMap[Var,Ty],tyvars:List[TyVar])
def emptyEnv() : Env = {
  Env(HashMap(),List())
}

def check(t: Term, env: Env) : Option[Ty] = {
  None // TODO
}

@main
def main() = {
  import Term.*
  import Ty.*
  val id = TyLambda("X",Lambda("x", VarTy("X"), Variable("x")));
  val id_ty = check(id, emptyEnv())
  assert(id_ty==ForallTy("X",FunTy(VarTy("X"),VarTy("X"))))

  val double = TyLambda("X", Lambda("f", FunTy(VarTy("X"),VarTy("X")), Lambda("a",VarTy("X"),App(Variable("f"), App(Variable("f"),Variable("a"))))))
  val double_ty = check(double, emptyEnv());

  assert(double_ty==ForallTy("X", FunTy(FunTy(VarTy("X"),VarTy("X")),FunTy(VarTy("X"),VarTy("X")))));
}
