module SnowFlakes where

import LineGraphics
import Fractals


{-
 -kochFlakes:: (Point, Point, Point) -> Int -> [Path]
 -kochFlakes baseTriangle maxLvl = kochFlakes' baseTriangle 1
 -    where kochFlakes' base lvl
 -        | lvl > maxLvl = []
 -        | otherwise =
 -}

splitLine::Line -> [Line]
splitLine line@((x1, y1), (x2, y2)) =
    [((x1,y1), p1) , (p2, (x2,y2))]
    where
        (_, p1) = scaleLine (1/3) line
        (_, p2) =  scaleLine (2/3) line


