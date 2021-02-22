oneToTen::[Int]
oneToTen = [1..10]

firstTenOdd::[Int]
firstTenOdd = [1,3..20]

-- infinite list
oneToInf:: [Int]
oneToInf = [1..]

-- cycled infinite list
cycledInf::[Int]
cycledInf = cycle [1,2,3]

-- take 5 cycledInf
-- drop 5 firstTenOdd 
--


-- list comprehensions
oneToTenDoubled::[Int]
oneToTenDoubled = [x*2 | x <- [1..10]]


-- only when x * 2 >= 12
oneToTenDoubledMod::[Int]
oneToTenDoubledMod = [x*2 | x <- [1..10], x * 2 >= 12]

