{-# LANGUAGE TypeApplications #-}
module Main where

import           Criterion.Main (bench, bgroup, defaultMain, nf)

import qualified Data.List      as Stl

import qualified MergeSortNub
import qualified SetNub
import qualified TreeSetNub

main :: IO ()
main = defaultMain $
  let testList = [(1 :: Int) ..(1200 :: Int)] ++ [(1 :: Int)..(1200 :: Int)] in
  [ bgroup "1200"
    [ bench "treeSet" $ nf TreeSetNub.nub       testList
    , bench "nub" $ nf Stl.nub        testList
    , bench "set" $ nf SetNub.nub testList
    , bench "mergeSort" $ nf (MergeSortNub.nub (<=)) testList
    ]
  ]
