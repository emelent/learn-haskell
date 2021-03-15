-- How many elements does it take for the sum of the roots of all natural numbers to exceed 1000? 
sqrtSums:: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1


-- $ operator
a = sum (map sqrt [1..130])
a' = sum $ map sqrt [1..130]


-- . operator, composes 2 expressions
-- f (g (x))
negx = map ($ 3) [(4+), (10*), (^2), sqrt]

-- it's clearer than lambdas
absNegate = map (\x -> negate (abs ((+3) x))) [5,-3,-6,7,-3,2,-19,24]  
absNegate' = map (negate . abs . (+3)) [5,-3,-6,7,-3,2,-19,24]  


-- another
another = map (\xs -> negate (sum (tail xs))) [[1..5],[3..6],[1..7]]   
another' = map (negate . sum . tail) [[1..5],[3..6],[1..7]]   