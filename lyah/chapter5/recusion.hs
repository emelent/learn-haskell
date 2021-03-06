maximum' :: Ord a => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
-- maximum' (x:xs)
--     | x > maxTail = x
--     | otherwise = maxTail
--     where maxTail = maximum' xs
maximum' (x : xs) = max x (maximum' xs)

repeat' :: a -> [a]
repeat' x = x : repeat' x

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys


quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a  <- xs, a > x]
    in smallerSorted ++ [x] ++ biggerSorted