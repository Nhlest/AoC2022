{-# LANGUAGE ScopedTypeVariables, TypeFamilies, DataKinds #-}
module Lib where

import Data.List
import Text.Parsec
import Text.Parsec.String
import Control.Applicative (liftA2)
import Data.Either
import Data.Char
import Data.List.Split

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
  data PreparedInput Day1 = PreparedInput { get :: [[Int]] }
  type Output Day1 Part1 = Int
  type Output Day1 Part2 = Int
  prepareInput _ = PreparedInput . (fmap . fmap) read . splitBy null . lines
  runPart1 _ = maximum . fmap sum . get
  runPart2 _ = sum . take 3 . reverse . sort . fmap sum . get

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy p s =  case dropWhile p s of
                 [] -> []
                 s' -> w : splitBy p s''
                       where (w, s'') = break p s'

data RPS = RockLose | PaperDraw | ScissorsWin deriving Show

parseRPS :: Parser [(RPS, RPS)]
parseRPS = 
  many $ liftA2 (,) (tok <$> anyChar <* char ' ') (tok <$> anyChar <* many newline)
 where tok 'A' = RockLose
       tok 'B' = PaperDraw
       tok 'C' = ScissorsWin
       tok 'X' = RockLose
       tok 'Y' = PaperDraw
       tok 'Z' = ScissorsWin
       tok _ = error "Unknown character"

data Day2 = Day2
instance Solution Day2 where
  data PreparedInput Day2 = PreparedInput2 { getInput2 :: [(RPS, RPS)] }
  type Output Day2 Part1 = Int
  type Output Day2 Part2 = Int
  prepareInput _ = PreparedInput2 . fromRight [] . runParser parseRPS () ""
  runPart1 _ = sum . map go . getInput2
   where score RockLose    = 1
         score PaperDraw   = 2
         score ScissorsWin = 3
         outcome RockLose RockLose       = 3
         outcome RockLose PaperDraw      = 6
         outcome RockLose ScissorsWin    = 0
         outcome PaperDraw RockLose      = 0
         outcome PaperDraw PaperDraw     = 3
         outcome PaperDraw ScissorsWin   = 6
         outcome ScissorsWin RockLose    = 6
         outcome ScissorsWin PaperDraw   = 0
         outcome ScissorsWin ScissorsWin = 3
         go (a, b) = score b + outcome a b
  runPart2 _ = sum . map go . getInput2
   where score RockLose    = 1
         score PaperDraw   = 2
         score ScissorsWin = 3
         outcome RockLose RockLose       = (0, ScissorsWin)
         outcome RockLose PaperDraw      = (3, RockLose)
         outcome RockLose ScissorsWin    = (6, PaperDraw)
         outcome PaperDraw RockLose      = (0, RockLose)
         outcome PaperDraw PaperDraw     = (3, PaperDraw)
         outcome PaperDraw ScissorsWin   = (6, ScissorsWin)
         outcome ScissorsWin RockLose    = (0, PaperDraw)
         outcome ScissorsWin PaperDraw   = (3, ScissorsWin)
         outcome ScissorsWin ScissorsWin = (6, RockLose)
         go (a, b) = let (score1, turn) = outcome a b in score1 + score turn

data Day3 = Day3
instance Solution Day3 where
  data PreparedInput Day3 = PreparedInput3 { getInput3 :: [String] }
  type Output Day3 Part1 = Int
  type Output Day3 Part2 = Int
  prepareInput _ = PreparedInput3 . lines
  runPart1 _ = sum . map go . getInput3
   where go line = points $ head $ intersect a b
          where len    = length line `div` 2
                (a, b) = splitAt len line
  runPart2 _ = sum . map go . chunksOf 3 . getInput3
   where go [a, b, c] = points $ head $ intersect a b `intersect` intersect b c

points c | c >= 'a' && c <= 'z' = ord c - ord 'a' + 1
         | c >= 'A' && c <= 'Z' = ord c - ord 'A' + 27