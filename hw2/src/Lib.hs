module Lib
    ( removeAt,
      collectEvery,
      stringSum,
      mergeSort
    ) where

import           Data.Char (digitToInt, isDigit)

removeAt :: Int -> [a] -> [a]
removeAt 0 (_:xs) = xs
removeAt _ [x]    = [x]
removeAt n (x:xs) = x : removeAt (n-1) xs

collectEvery :: Int -> [a] -> ([a], [a])
collectEvery n list | length list < (n - 1) = (list, [])

collectEvery n list = (x1 ++ x, y1:y)
                     where
                         (x1, tail_list) = splitAt (n - 1) list
                         y1 = list !! (n - 1)
                         (fst, second) = splitAt n list
                         (x, y) = collectEvery n (second)


stringSum :: String -> Int
stringSum str = strSum str 0 1
                where
                    strSum :: String -> Int -> Int -> Int
                    strSum (xs:x) num sign
                        | isDigit xs = strSum x (num * 10 + digitToInt xs) sign
                        | xs == '-' = if num == 0 then strSum x num (-1) else error "Unexpected sign"
                        | otherwise = num + strSum x 0 1
                    strSum [] num sign = num * sign

half :: [a] -> ([a], [a])
half xs = splitAt (length xs `div` 2) xs

mergeSort :: [Int] -> [Int]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort first) (mergeSort second)
                where
                    (first, second) = half xs

merge :: [Int] -> [Int] -> [Int]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
         | x <= y  = x:merge xs (y:ys)
         | otherwise = y:merge (x:xs) ys
