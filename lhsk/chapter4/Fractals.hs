module Fractals where

  -- needed to display the picture in the playground
import Codec.Picture

  -- our line graphics programming interface
import LineGraphics


house :: Path
house = [(300, 750), (300, 450), (270, 450), (500, 200),
         (730, 450), (700, 450), (700, 750)]

door :: Path
door = [(420, 750), (420, 550), (580, 550), (580, 750)]


tryItOut = drawPicture 2.0 [(lightgreen, house), (red, door)]

render fn = writePng "output.png" fn

{-
 -spiralRays:: Int -> Colour -> Line -> Picture
 -spiralRays n colour line@(p1, p2)
 -    | n <= 0 = []
 -    | otherwise = (colour, [p1, p2]): spiralRays (n - 1) newColour newLine
 -    where
 -        newColour = fade colour
 -        newLine = scaleLine 1.02 (rotateLine (pi / 40) line)
 -}

spiralRays :: Float -> Float -> Int -> Colour -> Line -> Picture
spiralRays angle scaleFactor n colour line
  = spiralRays' n colour line
  where
    spiralRays' n colour line@(p1, p2)
      | n <= 0 = []
      | otherwise = (colour, [p1, p2]) : spiralRays' (n-1) newColour newLine
      where
        newColour = if (mod n 3 == 0) then fade colour else colour
        newLine   = scaleLine scaleFactor (rotateLine angle line)

rotateLine:: Float -> Line -> Line
rotateLine alpha ((x1, y1), (x2, y2))
    = ((x1, y1), (x' + x1, y' + y1))
    where
        x0 = x2 - x1
        y0 = y2 - y1
        x' = x0 * cos alpha - y0 * sin alpha
        y' = x0 * sin alpha + y0 * cos alpha


fade :: Colour -> Colour
fade (r, g, b, opacity)
  = (r, g, b, opacity - 1)


scaleLine :: Float -> Line -> Line
scaleLine factor ((x1, y1), (x2, y2))
  = ((x1, y1), (x' + x1, y' + y1))
  where
    x0 = x2 - x1
    y0 = y2 - y1
    x' = factor * x0
    y' = factor * y0


connectLine:: Line -> Line -> Line
connectLine (_, pE) line2 = startLineFrom pE line2

startLineFrom :: Point -> Line -> Line
startLineFrom startPoint@(x0, y0) ((xS, yS), (xE, yE))
    = (startPoint, (x0 + (xE - xS), y0 + (yE - yS)))


spiral:: Float -> Float -> Int -> Line -> Path
spiral angle scaleFactor n line = spiral' n line
    where
        spiral' n line@(p1, p2)
            | n <= 0 = []
            | otherwise = p1 : spiral' (n - 1) newLine
            where
                newLine = connectLine line (scaleLine scaleFactor (rotateLine angle line))


polygon:: Int -> Line -> Path
polygon n line | n > 2 = spiral rotationAngle 1 (n + 1) line
    where
        rotationAngle = (2 * pi) / (fromIntegral n)
