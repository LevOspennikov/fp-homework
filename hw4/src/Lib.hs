module Lib
    ( Tree,
      empty,
      sizeTree,
      contains,
      insert,
      fromList,
      foldrTree,
      Sum,
      wow,
      sad,
      foldMapTree,
      splitOn
    ) where

data Tree a = Leaf | Node a (Tree a) (Tree a)
                deriving(Show)

instance Foldable Tree where
    foldr f z = foldrTree f z
    foldMap f m = foldMapTree f m

empty :: (Ord a) => Tree a -> Bool
empty Leaf = True
empty  _   = False

sizeTree :: Tree a -> Integer
sizeTree Leaf           = 0
sizeTree (Node _ t1 t2) = 1 + sizeTree t1 + sizeTree t2

contains :: (Ord a) => Tree a -> a -> Bool
contains Leaf _ = False
contains (Node v t1 t2) x
    | x == v = True
    | x < v = contains t1 x
    | x > v = contains t2 x

insert :: (Ord a) => Tree a -> a -> Tree a
insert Leaf x = Node x Leaf Leaf
insert (Node v t1 t2) x
    | v == x = Node v t1 t2
    | x < v = Node v (insert t1 x) t2
    | x > v = Node v t1 (insert t2 x)

fromList ::  (Ord a) => [a] -> Tree a
fromList []     = Leaf
fromList (xs:x) = insert (fromList x) xs

foldrTree :: (a -> b -> b) -> b -> Tree a -> b
foldrTree _ t Leaf = t
foldrTree fn base (Node v t1 t2) = foldrTree fn (foldrTree fn (fn v base) t1) t2


newtype Sum n = Sum n
                    deriving (Show)

instance Num n => Monoid (Sum n) where
  mempty = Sum 0
  mappend (Sum x) (Sum y) = Sum (x + y)

wow :: Int -> Sum Int
wow t = Sum t

sad :: Sum Int -> Int
sad (Sum x) = x

foldMapTree :: Monoid m => (a -> m) -> Tree a -> m
foldMapTree _ Leaf           = mempty
foldMapTree fn (Node v t1 t2) = mconcat [fn v, foldMapTree fn t1, foldMapTree fn t2]

splitOn :: (Eq a, Foldable t) => a -> t a -> [[a]]
splitOn sym = foldr (\c (x:xs) ->
    if c == sym
    then []:x:xs
    else (c:x):xs
  ) [[]]

