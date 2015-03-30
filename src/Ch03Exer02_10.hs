module Ch03Exer02_10 where

-- | 10. Write a function that calculates the turn made by three 2D points and returns a Direction.
data Direction = DirLeft | DirRight | DirStraight
         deriving (Show)
direction :: (Double,Double)->(Double,Double)->(Double,Double)->Direction
direction (x1,y1) (x2, y2) (x3,y3) = 
    direction' (x2-x1, y2-y1) (x3-x2, y3-y2)
    where
        direction' (x1, y1) (x2, y2)
            | sinTheta > 0   = DirLeft
            | sinTheta < 0   = DirRight
            | otherwise      = DirStraight
            where sinTheta = x1*y2 - y1*x2
--------------------------------------------------------------
-- | The solution's main entry point
solution :: IO ()
solution = do
    print $ direction (1,2) (2,3) (3,4)
    print $ direction (1,2) (2,3) (3,5)
    print $ direction (1,2) (2,3) (3,3)
    print $ direction (1,2) (2,3) (1,2)

-- | output 
-- DirStraight
-- DirLeft
-- DirRight
-- DirStraight