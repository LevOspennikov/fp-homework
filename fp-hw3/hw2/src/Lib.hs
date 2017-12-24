{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeOperators #-}

module Lib where

import           Control.Lens (Const (..), Identity (..), getConst, runIdentity)

data FS = Dir  { name     :: FilePath
               , contents :: [FS]
               }
        | File { name :: FilePath
               }
        deriving (Show)



type Lens s t a b = forall f . Functor f => (a -> f b) -> s -> f t
type Lens' s a = Lens s s a a
-- forall f . Functor f => (a -> f a) -> s -> f s
set :: Lens' s a -> a -> s -> s
set lens val st = runIdentity $ lens (\_ -> Identity val) st

view :: Lens' s a -> s -> a
view lens st = getConst $ lens (\x -> Const x) st

over :: Lens' s a -> (a -> a) -> s -> s
over lens f st = runIdentity $ lens (\x -> Identity $ f x) st

_1 :: Lens (a, x) (b, x) a b
_1 f (a, x) = (\ first -> (first, x)) <$> f a


_2 :: Lens (x, a) (x, b) a b
_2 f (x, a) = (\ sec -> (x, sec)) <$> f a
