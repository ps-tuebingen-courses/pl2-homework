import collection.mutable.HashMap;
import collection.mutable.HashSet;

type Variable = String;
type Location = Int;
type Store = HashMap[Location, Term];
type Env = HashMap[Variable, Type];

enum Type {
  case UnitTy
  case FunTy(from:Type,to:Type)
  case RefTy(ty:Type)
}

enum Term {
  case Var(v:Variable)
  case Lambda(v:Variable,annot:Type,body:Term)
  case App(fun:Term,arg:Term)
  case Unit
  case Ref(t:Term)
  case Deref(t:Term)
  case Assign(to:Term,body:Term)
  case Loc(loc:Location)
}

object Term{
  import Type.*
   
  def seq(t1:Term,t2:Term) : Term = {
    val vars = free_vars(t1) ++ free_vars(t2)
    val fresh = fresh_var(vars)
    App(Lambda(fresh,UnitTy,t2),t1)
     
  }

  def is_value(t:Term) : Boolean = {
    false //TODO
  }

  def free_vars(t:Term) : HashSet[Variable] = {
    HashSet() //TODO
  }

  def subst(t1:Term,v:Variable,t2:Term) : Term = {
    t1 match {
      case Var(v1) => if v1==v then t2  else { Var(v1) }
      case Lambda(v2,annot,body) => {
        val free_v = HashSet(v2,v) ++ free_vars(body);
        val fresh = fresh_var(free_v)
        val body_subst = subst(body,v2,Var(fresh))
        Lambda(fresh,annot,subst(body_subst,v,t2))
      }
      case App(fun,arg) => App(subst(fun,v,t2),subst(arg,v,t2))
      case Unit => Unit
      case Ref(t) => Ref(subst(t,v,t2))
      case Deref(t) => Deref(subst(t,v,t2))
      case Assign(to,body) => Assign(subst(to,v,t2),subst(body,v,t2))
      case Loc(loc) => Loc(loc)
    }
  }
}

def fresh_var(used_vars:HashSet[Variable]) : Variable = {
  var i =0;
  while(used_vars.contains("x"+i)) {
    i+=1
  };
  "x"+1
}

def fresh_loc(st:Store) : Location = {
  var i=0
  while(st.contains(i)){
    i+=1
  };
  i
}

def eval(t:Term,st:Store) : Option[Term] = {
  eval_once(t,st) match {
    case None => None
    case Some(t2) => if t == t2 then Some(t2)  else {eval(t,st)}
  }
}

def eval_once(t:Term,st:Store) : Option[Term] = {
  Some(t) //TODO
}

def check(t:Term,env:Env) : Option[Type] = {
  None //TODO
}

@main
def main() = {
  import Term.*
  import Type.* 
  val example1 = App(Lambda("x",RefTy(UnitTy),Deref(Var("x"))),App(Lambda("y",UnitTy,Ref(Var("y"))),Unit))
  val example2 = App(Lambda("x",RefTy(UnitTy),Assign(Var("x"),Deref(Var("x")))),Ref(Unit))
  val example1_checked = check(example1,HashMap());
  val example2_checked = check(example2,HashMap());
  val example1_evaled = eval(example1,HashMap());
  val example2_evaled = eval(example2,HashMap());
  assert(example1_checked == UnitTy)
  assert(example2_checked == UnitTy)
  assert(example1_evaled == Unit)
  assert(example2_evaled == Unit)
}
