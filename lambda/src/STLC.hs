module STLC where

import Lib

import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Data.List

type Name = String

data Expr = 
    Var Name
    | App Expr Expr
    | Lam Name Type Expr deriving (Show)

data Type = 
    O | Arrow Type Type deriving (Show, Eq)

type Ctx = [(Name, Type)]

-- check :: Expr -> Maybe Type
-- check 

-- findInCtx :: (Monad m) => Name -> Ctx -> MaybeT m Type
-- findInCtx x ctx = MaybeT $ return $ fmap snd $ find (\(y, t) -> x==y) ctx

findInCtx :: Name -> Ctx -> Maybe Type
findInCtx x = fmap snd . find (\(y, t) -> x==y)

checkInCtx :: Expr -> MaybeT (State Ctx) Type
checkInCtx (Var x) = lift get >>= hoistMaybe . findInCtx x
checkInCtx (App e1 e2) = do
    t1 <- checkInCtx e1
    case t1 of
        O -> empty
        Arrow a b -> do
            t2 <- checkInCtx e2
            if t2 == a then return b else empty
checkInCtx (Lam x t e) = Arrow t <$> (MaybeT $ with (x,t) $ runMaybeT $ checkInCtx e)

check :: Expr -> Maybe Type
check e = evalState (runMaybeT $ checkInCtx e) []

tt = Arrow O O

stlcTest :: IO ()
stlcTest = print $ check $ Lam "y" O $ Lam "x" tt $ App (Var "y") (Var "x")