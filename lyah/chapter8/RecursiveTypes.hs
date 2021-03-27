module RecursiveTypes
where

-- data List a = Empty | Cons a (List a) 
--     deriving (Show, Eq, Read, Ord)

-- data List a = Empty | a :-: (List a) 
--     deriving (Show, Eq, Read, Ord)

-- playing with infix
(%) :: Integral a => a -> a -> a
x % y = x `mod` y 

-- priority of 5 ( fixity)
infixr 5 :> 
data List a = Empty | a :> (List a) deriving (Show, Read, Eq, Ord)  

infix 5 .++
(.++):: List a -> List a -> List a
Empty .++ ys = ys
(x :> xs)  .++ ys = x :> (xs .++ ys)
-- ((:>) x xs)  .++ ys = x :> (xs .++ ys) -- this also works, pattern matching matches constructors


