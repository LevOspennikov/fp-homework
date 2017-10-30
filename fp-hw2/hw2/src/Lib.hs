module Lib
    ( bin,
      combinations,
      permutations
    ) where

bin :: Int -> [[Int]]
bin n | n == 0 = [[]]
      | n > 0 = (bin (n - 1)) >>= \set -> [1:set, 0:set]
      | otherwise  = error "Error"

combinations :: Int -> Int -> [[Int]]
combinations n k | k < 0 || n <= 0 || k > n = error "err"
                 | k == 0 = [[]]
                 | k == n = [[n, n - 1..1]]
                 | otherwise = (combinations (n - 1) (k - 1) >>= \set -> ([n:set])) ++ (combinations (n - 1) (k))

permutations :: [Integer] -> [[Integer]]
permutations arr | length arr == 1 = [arr]
                 | otherwise = concat (map (\(elem, num) -> permutations (deleteN num arr) >>= (\set -> [elem:set]) ) (zip arr [0..]))



deleteN :: Integer -> [a] -> [a]
deleteN _ []     = []
deleteN i (a:as)
   | i == 0    = as
   | otherwise = a : deleteN (i-1) as
