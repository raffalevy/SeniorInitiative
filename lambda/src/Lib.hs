module Lib where

import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Data.List

hoistMaybe :: (Monad m) => Maybe r -> MaybeT m r
hoistMaybe = MaybeT . return

with :: s -> State [s] r -> State [s] r
with x f = do
    modify (x:)
    y <- f
    modify tail
    return y