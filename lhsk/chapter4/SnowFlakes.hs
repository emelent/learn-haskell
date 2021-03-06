module SnowFlakes where

import Fractals
import LineGraphics


-- kochLine
kochLine :: Int -> Point -> Point -> Path
kochLine n pS pE
  | n <= 0 = []
  | otherwise =
    [pS] ++ kochLine (n - 1) pS p1
      ++ kochLine (n - 1) p1 p2
      ++ kochLine (n - 1) p2 p3
      ++ kochLine (n - 1) p3 pE
      ++ [pE]
  where
    l1@(_, p1) = scaleLine (1 / 3) (pS, pE)
    l2@(_, p3) = connectLine l1 l1
    (_, p2) = rotateLine (5 / 3 * pi) $ l2 -- what the heck does '$' do?

-- kochLine
kochLine' :: Int -> Point -> Point -> Path
kochLine' n pS pE
  | n <= 0 = []
  | otherwise =
    [pS] ++ kochLine' (n - 1) pS p1
      ++ kochLine' (n - 1) p1 p2
      ++ kochLine' (n - 1) p2 p3
      ++ kochLine' (n - 1) p3 pE
      ++ [pE]
  where
    l1@(_, p1) = scaleLine (1 / 3) (pS, pE)
    l2@(_, p3) = connectLine l1 l1
    (_, p2) = rotateLine (-(pi / 3)) l2


kochFlake:: Int -> Line -> Path
kochFlake n line
    = kochLine n p1 p2 ++ kochLine n p2 p3 ++ kochLine n p3 p1
    where
        [p1, p2, p3, _] = polygon 3 line

{-
-- Koch flakes attempt
kochFlakes :: [Line] -> Int -> [Path]
kochFlakes lines lvl = kochFlakes' lines 1
  where
    kochFlakes' :: [Line] -> Int -> [Path]
    kochFlakes' lines lvl
      | lvl == 0 = []
      | lvl == 1 = []
      | otherwise = []

splitLine::Line -> [Line]
splitLine line@((x1, y1), (x2, y2)) =
    [((x1,y1), p1) , (p2, (x2,y2))]
    where
        (_, p1) = scaleLine (1/3) line
        (_, p2) =  scaleLine (2/3) line

thirdPoint:: Point -> Point -> Point
thirdPoint p1@(x, y) p2 = (x + a, y - b)
    where
        a = (lineLength (p1, p2)) / 2
        b = a * tan (pi/3)

lineLength:: Line -> Float
lineLength ((x1, y1), (x2, y2)) = sqrt ( dx * dx + dy * dy)
    where
        dx = x2 - x1
        dy = y2 - y1
         -}
