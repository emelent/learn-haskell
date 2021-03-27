module TypeClasses
where

data TrafficLight = Red | Yellow | Green deriving(Read)

-- manually implement Eq interface, i.e. without deriving from it
instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False

instance Show TrafficLight where
    show Red = "Red light"
    show Yellow = "Yellow light"
    show Green = "Green light"

-- how do we instance Read? `read "Red"` to `Red`
-- instance Read TrafficLight where
--     readsPrec String "Red" = Red



-- Mimicking javascript's "truthy/falsey"-ness with yesno typeclass 
class YesNo a where
    yesno:: a -> Bool


instance YesNo Int where
    yesno 0 = False
    yesno _ = True

instance YesNo Double where
    yesno 0 = False
    yesno _ = True

instance YesNo Float where
    yesno 0 = False
    yesno _ = True

instance YesNo Bool where
    yesno b = b

instance YesNo [a] where
    yesno [] = False
    yesno _ = True

instance YesNo (Maybe a) where
    yesno (Just _) = True
    yesno Nothing = False

-- looks into contents of maybe and does yesno on that value
-- instance (YesNo a) => YesNo (Maybe a) where
--     yesno (Just x) = yesno x
--     yesno Nothing = False