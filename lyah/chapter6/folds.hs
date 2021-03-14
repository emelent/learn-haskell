-- string join
join:: [String] -> String
join = foldl (\a b -> a ++ " " ++ b) ""

-- split:: [String] -> String
-- split = foldl (\a b -> a ++ " " ++ b) ""

elem' :: (Eq a) => a -> [a] -> Bool  
elem' y = foldl (\acc x -> x == y || acc) False 

elem'' :: (Eq a) => a -> [a] -> Bool  
elem'' y = foldl (\acc x -> acc || x == y ) False 

-- 
reverse':: [a] -> [a]
-- reverse' = foldl (\acc x -> x: acc) []
reverse' = foldl (flip (:)) []