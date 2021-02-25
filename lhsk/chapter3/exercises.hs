-- Exercises Recursion
-- http://learn.hfm.io/recursion.html
--

module Exercises where

-- 1. Define the function length :: [a] -> Int. 
--
_length::[a] -> Int
_length [] = 0
_length (_:xs) = 1 + (_length xs)


-- 2. What are the values of the following expressions 
--    and what is wrong with the ones that give errors?
--

{- 
  1:[2,3,4]           => [1,2,3,4]
  1:2:3:4:[]          => [1,2,3,4]
  [1,2,3]:[4..7]      => ! error, the head part of cons to be type Int not [Int]
  [1,2,3] ++ [4..7]   => [1,3,4,5,6,7]
  1:['a','b']         => ! error, can't combine lists of different types
  "abc"++"cd"         => "abccd"
  "a":"bCc"           => ! error, expected head part of const to be Char not [Char]
  "a" ++ "bCc"        => "abCc"
  'a':'b'             => ! error, expected tail to be [Char] not Char
  'a':"b"             => "ab"
  [1,4,7] ++ 4:[5:[]] => ! error, [5:[]] results in [[Int]] and not [Int] so it can't be combined
  [True,True:[]]      => ! error, True:[] results in [Bool] and not Bool so it can't be added to List
  True:[True,False]   => [True, True, False]
 -}

-- 3. Write a recursive function fact to compute the factorial of a given 
--    positive number (ignore the case of 0 for this exercise).
--
_fact:: (Eq a, Num a) => a -> a
_fact x 
  | x == 1 = 1
  | otherwise = x * _fact (x - 1)

_fact':: Integral a => a -> a
_fact' 0 = 1
_fact' x = x * _fact (x - 1)
