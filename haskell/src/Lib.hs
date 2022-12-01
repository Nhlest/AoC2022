{-# LANGUAGE ScopedTypeVariables, TypeFamilies, DataKinds #-}
module Lib where

import Data.List

data Part1
data Part2

class Solution a where
  data PreparedInput a :: *
  type Output a part :: *
  prepareInput :: a -> String -> PreparedInput a
  runPart1 :: a -> PreparedInput a -> Output a Part1
  runPart2 :: a -> PreparedInput a -> Output a Part2
  runBothParts :: a -> String -> (Output a Part1, Output a Part2)
  runBothParts a input = (runPart1 a preparedInput, runPart2 a preparedInput)
    where preparedInput = prepareInput a input

data Day1 = Day1

instance Solution Day1 where
  data PreparedInput Day1 = PreparedInput { getInput :: [[Int]] }
  type Output Day1 Part1 = Int
  type Output Day1 Part2 = Int
  prepareInput _ = PreparedInput . (fmap . fmap) read . splitBy null . lines
  runPart1 _ = maximum . fmap sum . getInput
  runPart2 _ = sum . take 3 . reverse . sort . fmap sum . getInput

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy p s =  case dropWhile p s of
                 [] -> []
                 s' -> w : splitBy p s''
                       where (w, s'') = break p s'