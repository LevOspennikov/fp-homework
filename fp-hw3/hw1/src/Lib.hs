{-# LANGUAGE TemplateHaskell #-}


module Lib  where
import           Control.Monad
import           Language.Haskell.TH

chooseByIndices :: Int -> [Int] -> Q Exp
chooseByIndices n ids = do
    as <- replicateM n (newName "a")
    lamE [tupP (map varP as)] $ tupE (map (map varE as!!) ids)

