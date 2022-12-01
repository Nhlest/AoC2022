{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Lib

main :: IO ()
main = do
    input :: [[Int]] <- (fmap . fmap) read . splitBy null . lines <$> readFile "inputs/input"
    print $ run1 input
    print $ run2 input
    pure ()