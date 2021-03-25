module Record where

data Person = Person
  { firstName :: String
    ,lastName :: String
    ,age :: Int
    -- ,height :: Float
    -- ,phoneNumber :: String
    -- ,flavor :: String
  }
  deriving (Show, Eq, Ord)

-- data MyMap k v = MyMap [(k, v)]

data Vector a = Vector a a a | Hector a  deriving (Show) 

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i + l) (j + m) (k + n)
(Hector i) `vplus` (Hector l) = Hector (i + l)

vectMult :: (Num t) => Vector t -> t -> Vector t
(Vector i j k) `vectMult` m = Vector (i * m) (j * m) (k * m)

scalarMult :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `scalarMult` (Vector l m n) = i * l + j * m + k * n



data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday   
           deriving (Eq, Ord, Show, Read, Bounded, Enum)  