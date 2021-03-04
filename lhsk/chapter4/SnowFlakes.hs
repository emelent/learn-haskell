module SnowFlakes where

import LineGraphics
import Fractals



{-
 -kochFlakes:: [Line] -> Int -> [Path]
 -kochFlakes lines lvl = kochFlakes' lines 1
 -    where
 -        kochFlakes':: [Line] -> Int -> [Path]
 -        kochFlakes' lines lvl
 -        | lvl == 0 = []
 -        | lvl == 1 = []
 -        | otherwise = []
 -
 -}

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


