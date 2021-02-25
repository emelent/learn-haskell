module Recursion where

import Data.Char

natSum :: Int -> Int
natSum 0 = 0
natSum n
  | n > 0 = n + natSum (n - 1)
  | otherwise = error "natSum: Input value too small!"


{- Performing operations on lists of items, i.e. mapping -}


--
allSquares :: Num a=> [a] -> [a]
allSquares [] = []
allSquares (x: xs) = x * x : allSquares xs


allToUpper:: [Char] -> [Char]
allToUpper [] = []
allToUpper (c:cs) = toUpper c : allToUpper cs


type Point = (Int, Int)

distance::Point -> Point -> Float
distance (x1, y1) (x2, y2) = sqrt (fromIntegral (dx*dx + dy*dy))
  where 
    dx = x2 - x1
    dy = y2 - y1

distanceFromPoint:: Point -> [Point] -> [Float]
distanceFromPoint point [] = []
distanceFromPoint point (p:ps) = distance point p : distanceFromPoint point ps

{- Filtering list items -}

extractDigits :: String -> String
extractDigits [] = []
extractDigits (c: cs)
  | isDigit c = c : extractDigits cs
  | otherwise = extractDigits cs

inRadius point radius [] = []
inRadius point radius (p:ps) 
  | distance point p <= radius = p: inRadius point radius ps
  | otherwise = inRadius point radius ps


{- Reductions -}

prod:: Num a => [a] -> a
prod [] = 1
prod (x:xs) = x * (prod xs)


smallest:: Ord a => [a] -> a
smallest [] = error "Can't get the smallest of an empty list"
smallest (x:[]) = x
smallest (x:xs)
  | x <= smallest xs = x
  | otherwise = smallest xs 


minList :: [Int] -> Int
minList (x:[]) = x
minList (x:xs) = x `min` minList xs

-- turn an list of lists into 1 list
flatten:: [[a]] -> [a]
flatten [] = []
flatten (x: xs) = x ++ flatten xs


reverso:: [a]-> [a]
reverso [] = []
reverso xs = last xs : reverso (init xs)

reverso':: [a]-> [a]
reverso' [] = []
reverso' (x:xs) = reverso xs ++ [x]


-- deduct from balance
deductFromAccount::Num a => a -> [a] -> a
deductFromAccount bal [] = bal
deductFromAccount bal (x:xs) = deductFromAccount (bal - x) xs


deduct:: (Ord a, Num a, Show a) => a -> a -> a
bal `deduct` amt
  | bal < amt = error ("Amount " ++ show amt ++ " cannot be deducted from bal. " ++ show bal)
  | otherwise = bal - amt

deductFromAccount'::(Num a, Ord a, Show a) => a -> [a] -> a
deductFromAccount' bal [] = bal
deductFromAccount' bal (x:xs) = deductFromAccount' (bal `deduct` x) xs

deductFromAccount'' bal [] = bal
deductFromAccount'' bal (x:xs)
  | bal < x = error ("Amount " ++ show x ++ " cannot be deducted from bal. " ++ show bal)
  | otherwise = deductFromAccount'' (bal - x) xs


-- string to int
--
strToInt:: [Char] -> Int
strToInt [] = 0
strToInt (x:xs)
  | isDigit x =(10 ^ fromIntegral(length xs) * digitToInt x + (strToInt xs))
  | otherwise = error ("Cannot convert '" ++ show x ++ "' to int")


-- this one is by the book with accumulator
stringToInt :: String -> Int
stringToInt str = stringToIntAcc 0 str
  where
    stringToIntAcc:: Int -> [Char] -> Int
    stringToIntAcc acc [] = acc
    stringToIntAcc acc (c: str) 
      = stringToIntAcc (10 * acc + digitToInt c) str


-- left associative
--
fastReverse :: [a] -> [a]
fastReverse xs = reverseAcc [] xs
  where
    reverseAcc::[a] -> [a] -> [a]
    reverseAcc accList [] = accList
    reverseAcc accList (x:xs) = reverseAcc (x: accList) xs


-- combinations maps, filters and two forms of 
-- reducers(accumulator, non-accumulator)
--

-- sum of even numbers
--   filterEven, then sum the results
sumEvenElems:: (Integral a) => [a] -> a
sumEvenElems [] = 0
sumEvenElems xs = sum (filterEven xs)
  where
    filterEven::(Integral a) => [a] -> [a]
    filterEven [] = []
    filterEven (x:xs) 
      | x `mod` 2 == 0 = x : filterEven xs
      | otherwise = filterEven xs


sumEvenElems':: (Integral a) => [a] -> a
sumEvenElems' [] = 0
sumEvenElems' (x:xs)
  | x `mod` 2 == 0 = x + (sumEvenElems' xs)
  | otherwise = sumEvenElems' xs



type FPoint = (Float, Float)

fDistance::FPoint -> FPoint -> Float
fDistance (x1, y1) (x2, y2) = sqrt (dx*dx + dy*dy)
  where 
    dx = x2 - x1
    dy = y2 - y1


closestPoint:: FPoint -> [FPoint] -> FPoint
closestPoint o [] = error "No points given."
closestPoint o (p:ps) = closestPointAcc o p ps
  where 
    closestPointAcc:: FPoint -> FPoint -> [FPoint] -> FPoint
    closestPointAcc o cp [] = cp
    closestPointAcc o cp (p:ps)
      | fDistance o cp > fDistance o p = closestPointAcc o p ps
      | otherwise = closestPointAcc o cp ps


closestPoint':: FPoint -> [FPoint] -> FPoint
closestPoint' o [] = error "No points given."
closestPoint' o (p: []) = p
closestPoint' o (p:ps)
  | pDist < nDist = p
  | otherwise = closestPoint' o ps
  where
    pDist = fDistance o p
    nDist = fDistance o (closestPoint' o ps)

