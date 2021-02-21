-- return multiple values
addMul :: Num a => a -> a -> (a, a)
addMul x y = (x + y, x * y)

-- types
type Point = (Int, Int)

-- origin of point system
origin :: Point
origin = (0, 0)

-- move a given point to the right
--
moveRight :: Point -> Int -> Point
moveRight (x, y) distance = (x + distance, y)

-- move a given point up
--
moveUp :: Point -> Int -> Point
moveUp (x, y) distance = (x, y + distance)

-- represents colour string
--
type Colour = String

-- Colour points
--
type ColourPoint = (Int, Int, Colour)

-- move colour point vertically and horizontally
--
move :: ColourPoint -> Int -> Int -> ColourPoint
move (x, y, colour) xDistance yDistance =
  (x + xDistance, y + yDistance, colour)

-- compute distance between two Colour points
--
distance :: ColourPoint -> ColourPoint -> Float
distance (x1, y1, colour1) (x2, y2, colour2) =
  sqrt
    (fromIntegral (dx * dx + dy * dy))
  where
    dx = x2 - x1
    dy = y2 - y1
