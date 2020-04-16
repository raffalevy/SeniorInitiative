{-# LANGUAGE TemplateHaskell #-}

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
concrete (Arrow t1 t2) = TTArrow (concrete t2) (concrete t2)

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
        
natE = Atom "nat"
succE = Constant "succ" $ Arrow natE natE

infrTest :: IO ()
infrTest = print $ genConstraints $ uniquify $ Lam "x" $ App succE (Var "x")