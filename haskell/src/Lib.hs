module Lib where

import Data.List

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy p s =  case dropWhile p s of
                      [] -> []
                      s' -> w : splitBy p s''
                            where (w, s'') = break p s'

run1 :: [[Int]] -> Int
run1 = maximum . fmap sum 

run2 :: [[Int]] -> Int
run2 = sum . take 3 . reverse . sort . fmap sum 