{-# LANGUAGE NoImplicitPrelude #-}

import           Control.Monad
import           MonadFish
import           MonadJoin

instance Monad m => MonadFish m where
    returnFish = return
    f >=> g    = \x -> f x >>= g

instance Monad m => MonadJoin m where
    returnJoin = return
    join x     = x >>= id
