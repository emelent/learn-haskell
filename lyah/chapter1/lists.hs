oneToTen :: [Int]
oneToTen = [1 .. 10]

firstTenOdd :: [Int]
firstTenOdd = [1, 3 .. 20]

-- infinite list
oneToInf :: [Int]
oneToInf = [1 ..]

-- cycled infinite list
cycledInf :: [Int]
cycledInf = cycle [1, 2, 3]

-- take 5 cycledInf
-- drop 5 firstTenOdd
--

-- list comprehensions
oneToTenDoubled :: [Int]
oneToTenDoubled = [x * 2 | x <- [1 .. 10]]

-- only when x * 2 >= 12
oneToTenDoubledMod :: [Int]
oneToTenDoubledMod = [x * 2 | x <- [1 .. 10], x * 2 >= 12]

-- FizzBuzz

fizzBuzz' :: [Int] -> [String]
fizzBuzz' xs =
  [ if x `mod` 3 == 0 && x `mod` 5 == 0
      then "FizzBuzz"
      else
        if x `mod` 3 == 0
          then "Fizz"
          else
            if x `mod` 5 == 0
              then "Buzz"
              else show x
    | x <- xs
  ]

fizzBuzz :: [Int] -> [String]
fizzBuzz list = [fb x | x <- list]
  where
    fb n
      | n `mod` 3 == 0 && n `mod` 5 == 0 = "FizzBuzz"
      | n `mod` 3 == 0 = "Fizz"
      | n `mod` 5 == 0 = "Buzz"
      | otherwise = show n

xy = [x * y | x <- [2, 5, 10], y <- [8, 10, 11]]

nouns = ["hobo", "frog", "pope"]

adjectives = ["lazy", "grouchy", "scheming"]

phrases nouns adjectives = [adjective ++ " " ++ noun | adjective <- adjectives, noun <- nouns]

removeNonUpperCase phrase = [c | c <- phrase, c `elem` ['A' .. 'Z']]


-- triangles with list comprehensions
{- finding right angled triangle with perimeter of 24 -}
--
--
triangles x = [ (a, b, c) | c <- [1..x], b <- [1..x], a <- [1..x] ]
rightTriangles h = [ (a, b, c) | c <- [1..h], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2 ]
rightTriangles' h = [ (a, b, c) | c <- [1..h], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a + b + c == 24 ]

-- right angle triangle with max hypotenus h and perimeter p
--
-- rightTriangles'':: (Num, Eq a) => a -> a -> [(a, a, a)]
rightTriangles'' h p = [ (a, b, c) | (a, b, c) <- rightTriangles h, a + b + c == p ]
