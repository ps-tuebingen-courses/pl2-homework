import Data.Map as M;

type Var = String
type TyVar = String

data Term =
    Variable !Var
    | Lambda !Var !Type !Term
    | App !Term !Term
    | TyLambda !TyVar !Term
    | TyApp !Term !Type
  deriving(Show)

data Type = 
    VarTy !TyVar
    | FunTy !Type !Type 
    | ForallTy !TyVar !Type 
  deriving(Show)

data Env  = Env {
    vars:: !(Map Var Type),
    tyvars :: ![TyVar]
}

emptyEnv :: Env 
emptyEnv = Env { vars=M.empty, tyvars=[] }

check :: Term -> Env -> Maybe Type 
check _ _ = undefined --TODO

main :: IO () 
main = do 
    let id = TyLambda "X" (Lambda "x" (VarTy "X") (Variable "x"))
    let id_ty = check id emptyEnv
    let id_expected = ForallTy "X" (FunTy (VarTy "X") (VarTy "X"))
    print ("checked " <> show id_ty <> " expected " <> show id_expected)
            
    let double = TyLambda  "X"  (Lambda  "f" (FunTy (VarTy "X") (VarTy "X")) (Lambda "a" (VarTy "X") (App (Variable "f") (App (Variable "f") (Variable "a")))))
    let double_ty = check double emptyEnv
    let double_expected = ForallTy "X" (FunTy (FunTy (VarTy "X") (VarTy "X")) (FunTy (VarTy "X") (VarTy "X")))
    print ("checked " <> show double_ty <> " expected " <> show double_expected)
