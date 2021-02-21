_max :: Ord a => a -> a -> a
-- _max a b = if a >= b then a else b
_max x y
  | x >= y = x
  | otherwise = y

_signum :: (Ord a, Num a) => a -> Int
-- _signum a = if a < 0 then -1 else if x == 0 then 0 else 1
_signum x
  | x < 0 = -1
  | x == 0 = 0
  | otherwise = 1

circleArea :: Floating a => a -> a
circleArea radius = pi * radius * radius

circleArea' :: Floating a => a -> a
circleArea' diameter = pi * radius * radius
  where
    radius = diameter / 2.0 -- local variable

-- return multiple values
addMul :: Num a => a -> a -> (a, a)
addMul x y = (x + y, x * y)