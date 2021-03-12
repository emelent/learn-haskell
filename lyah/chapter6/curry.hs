compareWithHundered:: (Num a, Ord a) => a -> Ordering
compareWithHundered x = compare 100 x

compareWithHundered':: (Num a, Ord a) => a -> Ordering
compareWithHundered' = compare 100


divByTen :: Floating a => a -> a
divByTen = (/10)


tenOver :: Floating a => a -> a
tenOver = (10/)

tenOver' :: Floating a => a -> a
tenOver' = (/) 10


isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])


filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
    | f x = x : filter' f xs
    | otherwise = filter' f xs


quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = smaller ++ [x] ++ bigger
    where
        smaller = quickSort (filter (< x) xs)
        bigger = quickSort (filter (>= x) xs)
quicksort (x:xs) =     
    let smallerSorted = quicksort (filter (<=x) xs)  
        biggerSorted = quicksort (filter (>x) xs)   
    in  smallerSorted ++ [x] ++ biggerSorted  



-- find the sum of all odd squares that are smaller than 10,000.
someSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))  

someSum' = sum (takeWhile (<10000) [x^2 | x <- [1..], odd x])

-- collatz sequence
chain :: Integral a => a -> [a]
chain 1 = [1]
chain n
    | even n = n:chain (n `div` 2)
    | otherwise = n: chain (n*3 + 1)


numLongChains :: Int  
numLongChains = length (filter isLong (map chain [1..100]))  
    where isLong xs = length xs > 15  

numLongChains' :: Int
numLongChains' =  length [x | x <- [1..100], length (chain x) > 15]