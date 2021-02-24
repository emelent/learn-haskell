module Recursion where

import Data.Char

natSum :: Int -> Int
natSum 0 = 0
natSum n
  | n > 0 = n + natSum (n - 1)
  | otherwise = error "natSum: Input value too small!"


allSquares :: Num a=> [a] -> [a]
allSquares [] = []
allSquares (x: xs) = x * x : allSquares xs


allToUpper:: [Char] -> [Char]
allToUpper [] = []
allToUpper (c:cs) = toUpper c : allToUpper cs
