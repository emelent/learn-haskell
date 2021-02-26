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

-- 4. Write a recursive function enumFromTo which produces such 
--    a list given m and n, such that `enumFromTo m n = [m..n]`
--
_enumFromTo:: (Eq a, Ord a, Num a) => a -> a-> [a]
_enumFromTo m n
  | n < m = error "cannot increment down"
  | n == m = [m]
  | otherwise = m : _enumFromTo (m + 1) n
  

-- 5. Write a recursive function countOdds which calculates 
--    the number of odd elements in a list of Int values:
--      countOdds [1, 6, 9, 14, 16, 22] = 2 
--
_countOdds::Integral a => [a] -> Int
_countOdds [] = 0
_countOdds (x:xs)
  | x `mod` 2 /= 0 = 1 + (_countOdds xs) 
  | otherwise = (_countOdds xs)


-- 6. Write a recursive function removeOdd that, given a list 
--    of integers, removes all odd numbers from the list, e.g.,
--    removeOdd [1, 4, 5, 7, 10] = [4, 10]
--
removeOdd:: Integral a => [a] -> [a]
removeOdd [] = []
removeOdd (x:xs)
  | odd x = removeOdd xs
  | otherwise = x: removeOdd xs



-- 7. Challenge: At the end of the last screencast, demonstrating 
--    the implementation of 
--    closestPoint :: Point -> [Point] -> Point, 
--    we mentioned that the final implementation is less efficient 
--    than one might hope, as it uses the distance functions twice 
--    —instead of once— per recursive step. Improve the implementation 
--    to avoid that inefficiency.
--

type FPoint = (Float, Float)

fDistance::FPoint -> FPoint -> Float
fDistance (x1, y1) (x2, y2) = sqrt (dx*dx + dy*dy)
  where 
    dx = x2 - x1
    dy = y2 - y1


closestPoint:: FPoint -> [FPoint] -> FPoint
closestPoint o [] = error "No points given."
closestPoint o (p:ps) = closestPointAcc o (p, fDistance o p) ps
  where 
    closestPointAcc:: FPoint -> (FPoint,Float) -> [FPoint] -> FPoint
    closestPointAcc o (cp, dst) [] = cp
    closestPointAcc o (cp, dst) (p:ps)
      | dst > pDst = closestPointAcc o (p, pDst) ps
      | otherwise = closestPointAcc o (cp, dst) ps
        where pDst = fDistance o p
