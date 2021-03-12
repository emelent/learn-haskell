module PyTrees where

import Fractals
import LineGraphics


fractalTree :: Float -> Int -> Line -> Path
fractalTree factor n line = fractalTree' n line
  where
    fractalTree' 0 line = []  
    fractalTree' n line 
      = [p1, p4] ++ fractalTree' (n-1) (p4, p5) ++
                    fractalTree' (n-1) (p5, p3) ++
        [p3, p2] 
      where 
        -- flip direction of line
        flipLine :: Line -> Line
        flipLine (pS, pE) = (pE, pS)

        [p1,p2,p3,p4,_] = polygon 4 line
        r               = flipLine (scaleLine 0.5 (p3, p4))
        (_, p5)         = rotateLine (factor * pi) r 