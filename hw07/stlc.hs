import Data.Map as M
import Data.Maybe

type Id = String

data Term = 
    Var !Id 
    | Abs !Id !Term
    | App !Term !Term
    | Unit
  deriving( Show )

type UnificationVar = Int

data Ty = 
  ArrowTy !Ty !Ty 
  | UnitTy
  | VarTy !UnificationVar
  deriving ( Show )

freshVar :: [UnificationVar] -> UnificationVar
freshVar vars = fresh_var_i vars 0
 where 
   fresh_var_i :: [UnificationVar] -> UnificationVar -> UnificationVar
   fresh_var_i vars i = if i `elem` vars then fresh_var_i vars (i+1) else i

type Constraint = (Ty,Ty)

type Environment = M.Map Id Ty

type Unifier = M.Map UnificationVar Ty

typeCheck :: Environment -> Term -> Maybe Ty
typeCheck env t = do 
  let (ty,constrs) = collectConstraints env t
  case unify constrs of 
    Nothing -> Nothing
    Just unifier -> Just (applyUnifier unifier ty)


collectConstraints :: Environment -> Term -> (Ty,[Constraint])
collectConstraints = undefined --TODO

unify :: [Constraint] -> Maybe Unifier 
unify = undefined -- TODO

occurs :: UnificationVar -> Ty -> Bool
occurs = undefined -- TODO

substitute :: UnificationVar -> Ty -> Ty -> Ty 
substitute x by_ty (VarTy y) | x == y = by_ty 
substitute x by_ty (ArrowTy ty1 ty2) = ArrowTy (substitute x by_ty ty1) (substitute x by_ty ty2)
substitute _ _ other = other

substituteConstr :: UnificationVar -> Ty -> Constraint -> Constraint
substituteConstr x by_ty (ty1,ty2) = (substitute x by_ty ty2, substitute x by_ty ty2)

substituteConstrs :: UnificationVar -> Ty -> [Constraint] -> [Constraint]
substituteConstrs x by_ty = Prelude.map (substituteConstr x by_ty ) 


applyUnifier :: Unifier -> Ty -> Ty 
applyUnifier unifier (VarTy v)  = 
  case M.lookup v unifier of 
    Nothing -> VarTy v
    Just ty -> applyUnifier unifier ty 
applyUnifier unifier (ArrowTy ty1 ty2) = ArrowTy (applyUnifier unifier ty1) (applyUnifier unifier ty2)  
applyUnifier _ ty = ty

main = print $ typeCheck M.empty (App (Abs "x" (App (Var "x") (Var "x")))  (Abs "y" (Var "y")))
