module Ch03Exer02_09 where

-- | 9.Consider three two-dimensional points a, b, and c. 
--     If we look at the angle formed by the line segment from a to b and the line segment from b to c, 
--     it either turns left, turns right, or forms a straight line. 
--     Define a Direction data type that lets you represent these possibilities.

data Dir = DirLeft | DirRight | DirStraight
         deriving (Show)
--------------------------------------------------------------
-- | The solution's main entry point
solution :: IO ()
solution = do
    mapM_ print samples
    where
        samples = [DirLeft, DirRight, DirStraight]