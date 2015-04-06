module Ch03Exer02_12 where

import Data.List -- maximumBy
import Data.Ord  -- comparing
import Data.Tuple -- swap
import Data.Complex -- polar
    

data Direction = DirLeft | DirRight | DirStraight
         deriving (Show, Eq)

direction :: (Double,Double)->(Double,Double)->(Double,Double)->Direction
direction (x1,y1) (x2, y2) (x3,y3) =
    direction' (x2-x1, y2-y1) (x3-x2, y3-y2)
    where
        direction' (x1, y1) (x2, y2)
            | sinTheta > 0   = DirLeft
            | sinTheta < 0   = DirRight
            | otherwise      = DirStraight
            where sinTheta = x1*y2 - y1*x2

-- | 12. Using the code from the preceding three exercises, 
--       implement Graham's scan algorithm for the convex hull of a set of 2D points. 
--       You can find good description of what a convex hull. is,
--       and how the Graham scan algorithm should work, on Wikipedia.

grahamScan xs  | length xs < 2      = [] 
               | length convex' < 2 = [] 
               | otherwise          = convex'

  where (px,py)     = minimumBy (comparing swap) xs       -- find pivot

        vector      = [(x-px, y-py) | (x,y)<-xs]          -- to vector with pivot

        (v0:v1:vs)  = sortBy (comparing toRadian) vector  -- sort by radian 
            where toRadian (x,y) = snd $ polar (x :+ y)   -- polar :: (x,y) -> (length, theta)

        convex  = foldl add [v1,v0] (vs++[v0])            -- enlarge convex hull set 
            where add (c2:c1:cs) c3 | direction c1 c2 c3 == DirLeft     = (c3:c2:c1:cs) 
                                    | direction c1 c2 c3 == DirStraight = (c3:c1:cs) 
                                    | otherwise                         = add (c1:cs) c3 

        convex' = tail [(x+px, y+py) | (x,y)<-convex]     -- to point with pivot 

--------------------------------------------------------------
-- | The solution's main entry point
solution :: IO ()
solution = do
    print $ grahamScan [(1,2), (2,-1), (3,4), (5,-1), (6,7), (2, 2)]
    print $ grahamScan []
    print $ grahamScan [(1,2)]
    print $ grahamScan [(1,2), (2,3)]
    print $ grahamScan [(1,2), (2,3), (3,4)]
    print $ grahamScan [(1,2), (2,3), (3,3)]
