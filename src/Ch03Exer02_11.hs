module Ch03Exer02_11 where

-- | 11.Define a function that takes a list of 2D points and computes the direction of each successive triple. 
--      Given a list of points [a,b,c,d,e], 
--      it should begin by computing the turn made by [a,b,c], 
--      then the turn made by [b,c,d], then [c,d,e]. 
--      Your function should return a list of Direction.

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

directions []    = []
directions [a]   = []
directions [a,b] = []
directions (a:b:c:xs) = [(direction a b c)] ++ directions (b:c:xs)

--------------------------------------------------------------
-- | The solution's main entry point
solution :: IO ()
solution = do
    print $ directions [(1,2), (2,3), (3,4), (5,7), (6,7)]
    print $ directions [(1,2), (2,3), (3,4), (5,7)]
    print $ directions [(1,2), (2,3), (3,4)]
    print $ directions [(1,2), (2,3)]
    print $ directions [(1,2)]
    print $ directions []

-- | output 
-- [DirStraight,DirLeft,DirRight]
-- [DirStraight,DirLeft]
-- [DirStraight]
-- []
-- []
-- []