module Ch03Exer02_12 where
    
-- | 12. Using the code from the preceding three exercises, 
--       implement Graham's scan algorithm for the convex hull of a set of 2D points. 
--       You can find good description of what a convex hull. is,
--       and how the Graham scan algorithm should work, on Wikipedia.

grahamScan xs = True



--------------------------------------------------------------
-- | The solution's main entry point
solution :: IO ()
solution = do
    print $ grahamScan [(1,2), (2,3), (3,4), (5,7), (6,7)]
