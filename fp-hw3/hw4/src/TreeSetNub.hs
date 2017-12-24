module TreeSetNub (TreeSetNub.nub) where

import           Data.List     (nub, nubBy)
import qualified Data.Map      as Map
import qualified Data.Tree     as T
import qualified Data.Tree.Set as Set

nub :: (Ord a) => [a] -> [a]
nub [] = [];
nub (x:xs) = go (Set.singleton x) xs
  where
    go _ []     = []
    go s (x:xs) = if x `Set.elem` s then go s xs
                                      else x : go (Set.insertChild (Set.singleton x) s) xs
