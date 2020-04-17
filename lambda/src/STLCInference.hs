{-# LANGUAGE TemplateHaskell, TupleSections #-}

module STLCInference where

import Lib

import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Data.Maybe
import Data.List
import Control.Lens
import Control.Lens.TH

type Name = String

data Expr = 
    Var Name
    | App Expr Expr
    | Lam Name Expr 
    | Constant Name Type deriving (Show)

data Type = 
    Atom Name | Arrow Type Type deriving (Show, Eq)

data UExpr =
    UVar UID
    | UApp UExpr UExpr
    | ULam UID UExpr
    | UConstant Name Type deriving (Show)

data UniquifyCtx = UniquifyCtx { _uid :: UID, _stack :: [(Name, UID)]}
makeLenses ''UniquifyCtx

uniquify :: Expr -> UExpr
uniquify e = evalState (go e) $ UniquifyCtx {_uid = newUID, _stack = []} where
    go :: Expr -> State UniquifyCtx UExpr
    go (Var x) = uses stack $ UVar . snd . fromJust . find (\(y, t) -> x==y)
    go (App e1 e2) = UApp <$> go e1 <*> go e2
    go (Lam x e) = do
        id_x    <- zoom uid genUID
        stack   %= ((x,id_x):)
        e'      <- go e
        stack   %= tail
        return $ ULam id_x e'
    go (Constant a t) = return $ UConstant a t

data TTerm =
    TTAtom Name | TTArrow TTerm TTerm | TTHole UID deriving (Show, Eq)

concrete :: Type -> TTerm
concrete (Atom name) = TTAtom name
concrete (Arrow t1 t2) = TTArrow (concrete t1) (concrete t2)

solveConcrete :: TTerm -> Maybe Type
solveConcrete (TTAtom name) = Just $ Atom name
solveConcrete (TTArrow t1 t2) = Arrow <$> solveConcrete t1 <*> solveConcrete t2
solveConcrete (TTHole _) = Nothing

type Constraint = (TTerm, TTerm)

data GenConstraintsCtx = GenConstraintsCtx
    {_uidGC :: UID, _stackGC :: [(UID, UID)], _constraintsGC :: [Constraint]}
makeLenses ''GenConstraintsCtx

genConstraints :: UExpr -> (TTerm, [Constraint])
genConstraints e =
    let (t, s) = runState (go e) $ GenConstraintsCtx newUID [] [] in (t, _constraintsGC s) where
    go :: UExpr -> State GenConstraintsCtx TTerm
    go (UVar x) = uses stackGC $ TTHole . snd . fromJust . find (\(y, _) -> x==y)
    go (UApp e1 e2) = do
        t1 <- go e1
        t2 <- go e2
        x <- TTHole <$> zoom uidGC genUID
        constraintsGC %= ((t1, TTArrow t2 x):)
        return x
    go (ULam x e) = do
        tx <- zoom uidGC genUID
        stackGC %= ((x, tx):)
        te <- go e
        stackGC %= tail
        return $ TTArrow (TTHole tx) te
    go (UConstant a t) = return $ concrete t
        

subst :: UID -> TTerm -> TTerm -> TTerm
subst x u (TTAtom name) = TTAtom name
subst x u (TTArrow t1 t2) = TTArrow (subst x u t1) (subst x u t2)
subst x u (TTHole y) = if x==y then u else TTHole y

substConstraint :: UID -> TTerm -> Constraint -> Constraint
substConstraint x u (t1, t2) = (subst x u t1, subst x u t2)

occurs :: UID -> TTerm -> Bool
occurs x (TTAtom name) = False
occurs x (TTArrow t1 t2) = occurs x t1 || occurs x t2
occurs x (TTHole y) = x == y

unify :: [Constraint] -> Maybe [(UID, Type)]
unify cs = go cs [] where
    go :: [Constraint] -> [(UID, TTerm)] -> Maybe [(UID, Type)]
    go [] ds = sequence $ ds <&> \(x, t) -> (x,) <$> solveConcrete t
    go ((TTHole x, t2@(TTHole y)):cs) ds = if x == y then go cs ds
        else goWithSubst x t2 cs ds
    go ((t1@(TTArrow _ _), t2@(TTHole x)):cs) ds = go ((t2, t1):cs) ds
    go ((t1@(TTAtom _), t2@(TTHole x)):cs) ds = go ((t2, t1):cs) ds
    go ((TTArrow t1 t2, TTArrow u1 u2):cs) ds = go ((t1,u1):(t2,u2):cs) ds
    go ((TTHole x, t2@(TTAtom name)):cs) ds =
        goWithSubst x t2 cs ds
    go ((TTAtom name1, TTAtom name2):cs) ds = if name1==name2 then go cs ds else Nothing
    go ((TTAtom _, TTArrow _ _):cs) ds = Nothing
    go ((TTArrow _ _, TTAtom _):cs) ds = Nothing
    go ((TTHole x, t2@(TTArrow u1 u2)):cs) ds = if occurs x t2 then Nothing
        else goWithSubst x t2 cs ds

    goWithSubst :: UID -> TTerm -> [Constraint] -> [(UID, TTerm)] -> Maybe [(UID, Type)]
    goWithSubst x t2 cs ds = go (mapCS x t2 cs) ((x, t2):(mapDS x t2 ds))

    mapCS :: UID -> TTerm -> [Constraint] -> [Constraint]
    mapCS x u = fmap $ substConstraint x u

    mapDS :: UID -> TTerm -> [(UID, TTerm)] -> [(UID, TTerm)]
    mapDS x u = (fmap . fmap) (subst x u)

solveTTerm :: TTerm -> [(UID, Type)] -> Maybe Type
solveTTerm (TTAtom name) sol = Just $ Atom name
solveTTerm (TTArrow t1 t2) sol = Arrow <$> solveTTerm t1 sol <*> solveTTerm t2 sol
solveTTerm (TTHole x) sol = snd <$> find (\(y, _) -> x==y) sol

class Infer t where
    inferType :: t -> Maybe Type

instance Infer UExpr where
    inferType e = do
        let (t, cs) = genConstraints e
        sol <- unify cs
        solveTTerm t sol

instance Infer Expr where
    inferType = inferType . uniquify

natE = Atom "nat"
succE = Constant "succ" $ Arrow natE natE
plusE = Constant "plus" (Arrow natE (Arrow natE natE))
-- Just (Arrow (Arrow (Atom "nat") (Atom "nat")) (Arrow (Atom "nat") (Atom "nat")))
infrTest :: IO ()
-- infrTest = print $ inferType $ plusE
infrTest = print $ inferType $ Lam "x" $ Lam "y" $ App (App plusE (App succE (App (Var "y") (Var "x")))) (Var "x")
-- infrTest = do
--     let (t, cs) = genConstraints $ uniquify $ plusE
--     print t
--     print cs
--     print $ unify cs