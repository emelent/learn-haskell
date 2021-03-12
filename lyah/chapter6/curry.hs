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

