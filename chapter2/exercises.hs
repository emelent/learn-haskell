-- Exercises
-- http://learn.hfm.io/fundamentals.html

module Exercises where

-- Exercise 1
{-
  Write a function sort2 :: Ord a => a -> a -> (a, a) which accepts
  two Int values as arguments and returns them as a sorted pair, so
  that sort2 5 3 is equal to (3,5).
-}
--
sort2 :: Ord a => a -> a -> (a, a)
sort2 x y =
  if x <= y
    then (x, y)
    else (y, x)

-- Exercise 2
--
almostEqual :: Eq a => (a, a) -> (a, a) -> Bool
almostEqual (x1, y1) (x2, y2)
  | x1 == x2 = y1 == y2
  | x1 == y2 = y1 == x2
  | otherwise = False

almostEqual' :: Eq a => (a, a) -> (a, a) -> Bool
almostEqual' pair1 pair2 =
  (pair1 == pair2) || (swap pair1 == pair2)
  where
    swap (x, y) = (y, x)

-- Exercise 3
{-
  Define a function isLower :: Char -> Bool which returns True if
  a given character is a lower case letter.
-}
--
isLower :: Char -> Bool
isLower c = c `elem` ['a' .. 'z']

isLower' :: Char -> Bool
isLower' c = 'a' <= c && c <= 'z'

-- Exercise 4
{-
    Write a function mangle :: String -> String which
    removes the first letter of a word and attaches it at the end.
-}
--
mangle :: String -> String
mangle (x : y) = y ++ [x]
mangle [] = ""

-- Exercise 5
{-
  Implement division on Int, divide :: Int -> Int -> Int
  using the list functions described in this section.
-}
--
divide :: Int -> Int -> Int
divide x y = length [0, x .. y] - 1

divide' :: Int -> Int -> Int
divide' x y = length [x, x * 2 .. y]
