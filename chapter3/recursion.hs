module Recursion where

natSum :: Int -> Int
natSum 0 = 0
natSum n
  | n > 0 = n + natSum (n - 1)
  | otherwise = error "natSum: Input value too small!"
