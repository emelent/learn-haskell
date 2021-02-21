doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
    then x
    else x * 2

doubleSmallNumber' x = (if x > 100 then x else x*2) + 2

inc x = x + 1

conanO'Brien = "It's a me, Conan O'Brien!"

-- average:: Float -> Float -> Float
average:: Fractional a => a-> a -> a
average a b = (a + b) / 2.0


-- add:: Num a => a -> a -> a
add:: Int -> Int -> Int
add a b = a + b

-- add2:: Int -> Int -> Int  
-- add2:: Num a => a -> a -> a
add2 a b = a + b

doSomething :: Eq a => a -> a -> Bool
doSomething a b = a == b