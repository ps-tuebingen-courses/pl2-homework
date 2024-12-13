import Data.Map as M;
import Data.Set as S;

type Variable = String
type Location = Int
type Store = M.Map Location Term
type Env = M.Map Variable Type

data Type = 
    UnitTy
    | Fun !Type !Type 
    | RefTy !Type
  deriving (Show, Eq)


data Term =
    Var !Variable 
    | Lambda !Variable !Type !Term
    | App !Term !Term
    | Unit
    | Ref !Term 
    | Deref !Term
    | Assign !Term !Term
    | Loc !Location 
  deriving (Show, Eq)

seq :: Term -> Term -> Term 
seq t1 t2 = App (Lambda newV UnitTy t2) t1 
  where 
    newV = freshVar vars 
    vars = S.union (freeVars t1) (freeVars t2)

isValue :: Term -> Bool
isValue _ = undefined -- TODO

freeVars :: Term -> S.Set Variable
freeVars _ = undefined -- TODO

subst :: Term -> Variable -> Term -> Term
subst (Var v) var t = if v == var then t else Var v
subst (Lambda var annot body) v t = Lambda freshV annot (subst bodySubst v t)
  where 
    freshV = freshVar vars 
    vars = S.union (freeVars body) (S.singleton var)
    bodySubst = subst body var (Var freshV)
subst (App fun arg) v t = App (subst fun v t) (subst arg v t)
subst Unit _ _ = Unit 
subst (Ref t) v t' = Ref (subst t v t')
subst (Deref t) v t' = Deref (subst t v t')
subst (Assign to body) v t = Assign (subst to v t) (subst body v t)
subst (Loc loc) _ _ = Loc loc

freshVar :: S.Set Variable -> Variable
freshVar usedVars = freshVari usedVars 0
  where 
    freshVari :: S.Set Variable -> Integer -> Variable
    freshVari usedVars i = if ("x" <> show i) `elem` usedVars then freshVari usedVars (i+1) else "x"<> show i

freshLoc :: Store -> Location 
freshLoc st = freshLoci st 0
  where 
    freshLoci :: Store -> Location -> Location 
    freshLoci st i = if i `M.member` st then freshLoci st (i+1) else i

eval :: Term -> Store -> Maybe Term 
eval t st = case evaled of
  Just t' -> if t' == t then evaled else eval t' st
  Nothing -> Nothing
  where evaled = evalOnce t st 

evalOnce :: Term -> Store -> Maybe Term
evalOnce _ _ = undefined -- TODO

check :: Term -> Env -> Maybe Type 
check _ _ = undefined -- TODO

main :: IO () 
main = do 
  let example1 = App (Lambda "x" (RefTy UnitTy) (Deref (Var "x"))) (App (Lambda "y" UnitTy (Ref (Var "y"))) Unit)
  let example2 = App (Lambda "x" (RefTy UnitTy) (Deref (Var "x"))) (App (Lambda "y" UnitTy (Ref (Var "y"))) Unit)
  let example1Checked = check example1 M.empty
  let example2Checked = check example2 M.empty 
  let example1Evaled = eval example1 M.empty 
  let example2Evaled = eval example2 M.empty
  print (show example1Checked <> " should be " <> show UnitTy)
  print (show example2Checked <> " should be " <> show UnitTy)
  print (show example1Evaled  <> " should be " <> show Unit)
  print (show example2Evaled  <> " should be " <> show Unit)

