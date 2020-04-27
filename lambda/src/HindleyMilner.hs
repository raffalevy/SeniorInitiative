{-# LANGUAGE TemplateHaskell, TupleSections #-}

module HindleyMilner where

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
    | Let Name Expr Expr deriving (Show)

data MonoType =
    TVar Name
    | TApp Name [MonoType] deriving (Show, Eq)

data Type = 
    Mono MonoType
    | Poly Name Type deriving (Show, Eq)


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

