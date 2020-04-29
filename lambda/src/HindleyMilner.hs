{-# LANGUAGE TemplateHaskell, TupleSections, PatternSynonyms #-}

-- | Implementation of Hindley-Milner Type Inference

module HindleyMilner where

import Lib

import Control.Monad
import Control.Applicative
import Data.Functor
import Data.Maybe
import Data.List
import Text.Show

type Name = String

-- | An expression; carries no type information
data Expr = 
    Var Name -- ^ a variable
    | Expr :$ Expr -- ^ the application of one expression to another
    | Lam Name Expr -- ^ an anonymous function
    | Let Name Expr Expr -- ^ a let-expression; let x = _ in _

infixl 6 :$
pattern App e1 e2 = e1 :$ e2

-- | A quantifier-free type
data MonoType =
    TVar UID
    | MonoType :-> MonoType
    | TApp Name [MonoType] deriving (Eq)

infixr 6 :->
pattern TArrow t1 t2 = t1 :-> t2

-- | A type which may contain one or more quantifiers
data Type = 
    Mono MonoType -- ^ quantifier-free
    | Forall UID Type -- ^ polymorphic
        deriving (Eq)

instance Show MonoType where
    showsPrec d (TVar x) = showsPrec 11 x
    showsPrec d (t1 :-> t2) = showParen (d > 6) $
        showsPrec 7 t1 . showString " -> " . showsPrec 7 t2
    showsPrec d (TApp f ts) = showParen (d > 10) $
        showString f . showListWith (\t -> showsPrec 11 t) ts

instance Show Type where
    showsPrec d (Mono t) = showsPrec d t
    showsPrec d (Forall x t) = showParen (d > 0) $ showString "\x2200" . showsPrec 1 x . showString ". " . showsPrec 1 t

displayExpr :: Expr -> String
displayExpr (Var name) = name
displayExpr (Lam param body) = "\955" ++ param ++ "." ++ displayExpr body
displayExpr (App a b) = arg1 a ++ " " ++ arg2 b
    where
        arg1 a@(Var _) = displayExpr a
        arg1 a@(App _ _) = displayExpr a
        arg1 a@(Lam _ _) = "(" ++ displayExpr a ++ ")"
        arg1 a@(Let _ _ _) = "(" ++ displayExpr a ++ ")"
        
        arg2 b@(Var _) = displayExpr b
        arg2 b@(App _ _) = "(" ++ displayExpr b ++ ")"
        arg2 b@(Lam _ _) = "(" ++ displayExpr b ++ ")"
        arg2 b@(Let _ _ _) = "(" ++ displayExpr b ++ ")"
displayExpr (Let x a b) = "let " ++ x ++ " = " ++ displayExpr a ++ " in " ++ displayExpr b

type Context = [(Name, Type)]

type Constraint = (MonoType, MonoType)

newtype GCError = UnboundVariable Name
    deriving (Show)

type GCResult r = Either GCError r

substVarMono :: UID -> UID -> MonoType -> MonoType
substVarMono x y (TVar a) = if a == x then TVar y else TVar a
substVarMono x y (t1 :-> t2) = substVarMono x y t1 :-> substVarMono x y t2
substVarMono x y (TApp f ts) = TApp f $ substVarMono x y <$> ts

substVar :: UID -> UID -> Type -> Type
substVar x y (Mono t) = Mono $ substVarMono x y t
substVar x y (Forall a t) = Forall a $ if a == x then t else substVar x y t

instantiate :: Type -> UID -> (MonoType, UID)
instantiate (Mono t) uid = (t, uid)
instantiate (Forall a t) uid = instantiate (substVar a uid t) (uid + 1)

genConstraints :: Expr -> Context -> GCResult (MonoType, [Constraint])
genConstraints e ctx = go e ctx 0 <&> (\(a,b,c) -> (a,b)) where
    go :: Expr -> Context -> UID -> GCResult (MonoType, [Constraint], UID)
    go (Var x) ctx uid = fromMaybe (Left $ UnboundVariable x) $ do
        pt <- lookup x ctx
        let (mt, uid') = instantiate pt uid
        return (Right (mt, [], uid))
    go (App e1 e2) ctx uid = do
        (t1, c1, uid) <- go e1 ctx uid
        let c = c1

        (t2, c2, uid) <- go e2 ctx uid
        let c' = c2 ++ c

        let x = TVar uid
        let uid' = uid + 1
        let c'' = (t1, t2 :-> x):c'

        return (x, c'', uid')
    go (Lam x expr) ctx uid = do
        let tx = TVar uid
        let uid' = uid + 1
        (te, ce, uid') <- go expr ((x, Mono tx):ctx) uid'
        return $ (tx :-> te, ce, uid')
    go (Let x e1 e2) ctx uid = do
        (t1, c1, uid) <- go e1 ctx uid
        (t2, c2, uid) <- go e2 ((x, Mono t1):ctx) uid
        return (t2, c2 ++ c1, uid)

data UnifyError =
    Infinite MonoType MonoType
    | DiffConstructor MonoType MonoType
    | DiffArity MonoType MonoType
    deriving (Show)

type UnifyResult r = Either UnifyError r

type Substitution = [(UID, MonoType)]

subst :: UID -> MonoType -> MonoType -> MonoType
subst x t (TVar y) = if x == y then t else (TVar y)
subst x t (t1 :-> t2) = subst x t t1 :-> subst x t t2
subst x t (TApp f ts) = TApp f $ subst x t <$> ts

occurs :: UID -> MonoType -> Bool
occurs x (TVar y) = x == y
occurs x (t1 :-> t2) = occurs x t1 || occurs x t2
occurs x (TApp f ts) = any (occurs x) ts

elimFromSubstitution :: UID -> MonoType -> Substitution -> Substitution
elimFromSubstitution x t sub = sub <&> \(y,t1) -> (y, subst x t t1)

elimFromConstraints :: UID -> MonoType -> [Constraint] -> [Constraint]
elimFromConstraints x t cs = cs <&> \(t1, t2) -> (subst x t t1, subst x t t2)

elim :: UID -> MonoType -> [Constraint] -> Substitution -> ([Constraint], Substitution)
elim x t cs sub = (elimFromConstraints x t cs, (x,t) : elimFromSubstitution x t sub)

unify :: [Constraint] -> UnifyResult Substitution
unify cs = go cs [] where
    go :: [Constraint] -> Substitution -> UnifyResult Substitution
    go [] sub = Right sub
    go (c:cs) sub = u c cs sub

    u :: Constraint -> [Constraint] -> Substitution -> UnifyResult Substitution
    u (TVar x, TVar y) cs sub = if x == y then go cs sub else
        let (cs', sub') = elim x (TVar y) cs sub in go cs' sub'
    u (TVar x, t) cs sub = if occurs x t then Left $ Infinite (TVar x) t else
        let (cs', sub') = elim x t cs sub in go cs' sub'
    u (t, TVar x) cs sub = u (TVar x, t) cs sub
    u (t1 :-> t2, t3 :-> t4) cs sub = go ((t1,t3):(t2,t4):cs) sub
    u (a@(TApp f t1s), b@(TApp g t2s)) cs sub =
        if f == g then
            if length t1s == length t2s then
                go (zip t1s t2s ++ cs) sub
            else Left $ DiffArity a b
        else Left $ DiffConstructor a b
    u (a,b) cs sub = Left $ DiffConstructor a b

applySubstitution :: Substitution -> MonoType -> MonoType
applySubstitution sub (TVar x) = fromMaybe (TVar x) $ lookup x sub
applySubstitution sub (t1 :-> t2) = applySubstitution sub t1 :-> applySubstitution sub t2
applySubstitution sub (TApp f ts) = TApp f $ applySubstitution sub <$> ts

vars :: MonoType -> [UID]
vars t = nub $ go t where
    go (TVar x) = [x]
    go (t1 :-> t2) = go t1 ++ go t2
    go (TApp f ts) = concat $ go <$> ts

generalize :: MonoType -> Type
generalize t = foldr Forall (Mono t) $ vars t

data TypeError =
    GCError GCError
    | UnifyError UnifyError deriving (Show)

type InferResult r = Either TypeError r

-- | Infers the type of an expression given a set of typing assumptions.
infer :: Expr -> Context -> InferResult Type
infer expr ctx = case genConstraints expr ctx of
    Right (t,c) -> case unify c of
        Right sub -> Right $ generalize $ applySubstitution sub t
        Left err -> Left $ UnifyError $ err
    Left err -> Left $ GCError $ err