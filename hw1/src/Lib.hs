module Lib
       ( order3,
         highestBit,
         smartReplicate,
         contains
       ) where

order3 :: (Int, Int, Int) -> (Int, Int, Int)
order3 (x, y, z) = (min (min x y) z, max (min x y) (max (min x z) (min z y)), max (max x y) z)

highestBit :: Integer -> (Integer, Integer)
highestBit x | x <= 0 = error "Error"
             | x == 1 = (1, 0)
             | otherwise  = (2 * t, p + 1)
                                where
                                    (t, p) = highestBit (x `div` 2)

smartReplicate :: [Int] -> [Int]
smartReplicate []     = []
smartReplicate [xs]   = replicate xs xs
smartReplicate (xs:x) = smartReplicate [xs] ++ smartReplicate x

contains :: Int -> [[Int]] -> [[Int]]
contains n = filter (elem n)
