firstTenPrimes :: [Int]
firstTenPrimes = [2, 3, 5, 7, 11, 17, 19, 23, 29]

oneToTwenty :: [Int]
oneToTwenty = [1 .. 20]

firstTenOdd :: [Int]
firstTenOdd = [1, 3 .. 20]

lowerAToZ :: [Char]
lowerAToZ = ['a' .. 'z']

upperAToZ :: [Char]
upperAToZ = ['A' .. 'Z']

myHead :: [a] -> a
myHead (x : _) = x
myHead [] = error "Can't handle an empty list"

myTail :: [a] -> [a]
myTail (_ : x) = x
myTail [] = error "Can't handle an empty list"

-- Figure this one out later
-- listContains :: (Foldable a, Eq b) => a -> a b -> Bool
-- listContains list x = elem x list