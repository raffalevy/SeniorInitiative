module Untyped where

import Control.Monad
import Control.Applicative
import Control.Monad.Trans.State
import Data.List
import Data.Maybe

type Name = String

data Expr = 
    Var Name
    | App Expr Expr
    | Lam Name Expr deriving (Show)

zeroE = Lam "f" $ Lam "x" $ Var "x"
succE = Lam "n" $ Lam "f" $ Lam "x" $ App (Var "f") (App (App (Var "n") (Var "f")) (Var "x"))
oneE = App succE zeroE

omegaCombE = Lam "x" $ App (Var "x") (Var "x")
omegaE = App omegaCombE omegaCombE

displayExpr :: Expr -> String
displayExpr (Var name) = name
displayExpr (Lam param body) = "\955" ++ param ++ "." ++ displayExpr body
displayExpr (App a b) = arg1 a ++ " " ++ arg2 b
    where
        arg1 a@(Var _) = displayExpr a
        arg1 a@(App _ _) = displayExpr a
        arg1 a@(Lam _ _) = "(" ++ displayExpr a ++ ")"
        
        arg2 b@(Var _) = displayExpr b
        arg2 b@(App _ _) = "(" ++ displayExpr b ++ ")"
        arg2 b@(Lam _ _) = "(" ++ displayExpr b ++ ")"

data DB =
    DBVar Int
    | DBApp DB DB
    | DBLam DB deriving (Show)

displayDB :: DB -> String
displayDB (DBVar i) = show i
displayDB (DBLam body) = "\955 " ++ displayDB body
displayDB (DBApp a b) = arg1 a ++ " " ++ arg2 b
    where
        arg1 a@(DBVar _) = displayDB a
        arg1 a@(DBApp _ _) = displayDB a
        arg1 a@(DBLam _) = "(" ++ displayDB a ++ ")"
        
        arg2 b@(DBVar _) = displayDB b
        arg2 b@(DBApp _ _) = "(" ++ displayDB b ++ ")"
        arg2 b@(DBLam _) = "(" ++ displayDB b ++ ")"

exprToDB :: Expr -> DB
exprToDB expr = evalState (go expr) []
    where
        go :: Expr -> State [Name] DB
        go (Var name) = (DBVar . fromJust . elemIndex name) <$> get
        go (App expr1 expr2) = DBApp <$> go expr1 <*> go expr2
        go (Lam param body) = modify (param:) *> (DBLam <$> go body) <* modify tail

dbToExpr :: DB -> Expr
dbToExpr db = evalState (go db) (-1)
    where
        go :: DB -> State Int Expr
        go (DBVar i) = (Var . show . subtract i) <$> get
        go (DBApp a b) = App <$> go a <*> go b
        go (DBLam body) =
            modify (+1) *> (Lam <$> (show <$> get) <*> go body) <* modify (subtract 1)


subs_index :: DB -> Int -> DB -> DB
subs_index (DBVar j) i expr = if i == j then expr else (DBVar j)
subs_index (DBApp expr1 expr2) i expr =
    DBApp (subs_index expr1 i expr) (subs_index expr2 i expr)
subs_index (DBLam body) i expr = DBLam (subs_index body (i+1) expr)

beta :: DB -> Maybe DB
beta (DBVar i) = Nothing
beta (DBLam body) = DBLam <$> beta body
beta (DBApp (DBLam body) arg) = Just $ subs_index body 0 arg
beta (DBApp a b) = (DBApp <$> beta a <*> return b) <|> (DBApp <$> return a <*> beta b)

reduce :: DB -> DB
reduce expr = fromMaybe expr (reduce <$> beta expr)

reduce_n :: Int -> DB -> Maybe DB
reduce_n 0 expr = Nothing
reduce_n n expr = (beta expr >>= reduce_n (n-1)) <|> return expr

test :: IO ()
test = putStrLn $ displayExpr $ dbToExpr $ exprToDB $ succE