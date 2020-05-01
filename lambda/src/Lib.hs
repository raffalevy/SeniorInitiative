module Lib where

import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Data.List

hoistMaybe :: (Monad m) => Maybe r -> MaybeT m r
hoistMaybe = MaybeT . return

with :: (Monad m) => s -> StateT [s] m r -> StateT [s] m r
with x f = do
    modify (x:)
    y <- f
    modify tail
    return y

type UID = Word

type UIDT m r = StateT UID m r

{-# INLINE newUID #-}
newUID :: UID
newUID = 0

genUID :: (Monad m) => UIDT m UID
genUID = get <* modify (+1)

fmapl :: (a -> b) -> Either a c -> Either b c
fmapl f (Left x) = Left $ f x
fmapl _ (Right x) = Right x