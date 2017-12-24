module SetNub (SetNub.nub) where

import           Data.List (nub, nubBy)
import qualified Data.Set  as Set
import qualified Data.Tree as T

nub :: (Ord a) => [a] -> [a]
nub l = go Set.empty l
  where
    go _ []     = []
    go s (x:xs) = if x `Set.member` s then go s xs
                                      else x : go (Set.insert x s) xs
