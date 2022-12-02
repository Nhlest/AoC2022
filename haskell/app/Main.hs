{-# LANGUAGE ScopedTypeVariables, TypeFamilies, DataKinds #-}
module Main (main) where

import Lib

main :: IO ()
main = do
  input <- readFile "../inputs/day01"
  print $ (runBothParts Day1 input)
  input <- readFile "../inputs/day02"
  print $ (runBothParts Day2 input)
  pure ()