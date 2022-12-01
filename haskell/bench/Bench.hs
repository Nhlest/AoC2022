{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Lib
import Criterion
import Criterion.Main (defaultMain)

main :: IO ()
main = do
  input :: [[Int]] <- (fmap . fmap) read . splitBy null . lines <$> readFile "inputs/input"
  defaultMain
    [ bgroup "Benchmark Day 1" 
      [ bench "Benchmark part 1" $ whnf run1 input
      , bench "Benchmark Part 2" $ whnf run2 input
      ]
    ]