import Data.List
import Data.Char

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack =
  let nlen = length needle
   in foldl (\acc x -> (take nlen x == needle) || acc) False (tails haystack)

search' :: (Eq a) => [a] -> [a] -> Bool
search' needle haystack = needle `isInfixOf` haystack


ceasarEncode:: Int -> String -> String
ceasarEncode shift msg = map (chr . (+shift) . ord) msg

ceasarDecode:: Int -> String -> String
ceasarDecode shift msg = map (chr . subtract shift . ord) msg


-- Maps and such
--

phoneBook = [("betty","555-2938")  ,("bonnie","452-2928")  ,("patsy","493-2928"), ("patsy","123-1323") , ("patsy","838-7278")    ,("lucille","205-2928")  ,("wendy","939-8282")  ,("penny","853-2492")  ]

findKey::(Eq k) => k -> [(k,v)] -> Maybe v
findKey key [] = Nothing
findKey key ((k,v):xs) =
    if key == k
        then Just v
        else findKey key xs

findKey'::(Eq k) => k -> [(k,v)] -> Maybe v
findKey' key = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing
