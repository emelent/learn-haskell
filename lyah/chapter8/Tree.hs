module Tree
where

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton::a -> Tree a
singleton x = Node x EmptyTree EmptyTree


treeInsert:: Ord a => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a = Node a (treeInsert x left) right
    | x > a = Node a left (treeInsert x right)

treeElem:: Ord a => a -> Tree a -> Bool
treeElem _ EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a = treeElem x left
    | x > a = treeElem x right

-- messing around with getting parent of tree
-- there must be some way to do this using folds... what is a Foldable anyways?
treeParent:: Ord a => a -> Tree a -> Maybe a
treeParent x tree= treeParent' Nothing x tree
    where
        treeParent':: Ord a => Maybe a -> a -> Tree a -> Maybe a
        treeParent' _ _ EmptyTree = Nothing
        treeParent' parent x (Node a left right)
            | x == a = parent
            | x < a = treeParent' (Just a) x left
            | x > a = treeParent' (Just a) x right

sampleTree = foldr treeInsert EmptyTree [8, 6, 4, 1, 7, 3, 5]