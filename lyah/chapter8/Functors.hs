module Functors
where

import RecursiveTypes
import Tree


-- Functor typeclass def
-- class Functor f where  
--     fmap :: (a -> b) -> f a -> f b  
-- 


instance Functor List where
    fmap f Empty = Empty
    fmap f (x :> xs) = f x :> fmap f xs
    
instance Functor Tree where
    fmap f EmptyTree = EmptyTree 
    fmap f (Node a left right) = Node (f a) (fmap f left) (fmap f right)

-- Either instanced as Functor
-- data Either a b = Left a | Right b

-- `Either` type constructor takes 2 types, we partially apply the first,
-- then we give the last on to Functor, since Functor needs a type constructor
-- that accepts 1 type

-- instance Functor (Either a) where  
--     fmap f (Right x) = Right (f x)  
--     fmap f (Left x) = Left x  