module Ch04Exer02_03 where
-------------
import Prelude hiding (concat)
-------------------------------------------------------------
concat :: [[a]] -> [a]
concat xs = foldr (++) [] xs
-------------------------------------------------------------
-- | The solution's main entry point
solution :: IO ()
solution = do
    print $ concat [[0], [1], [2], [3]]
    print $ concat [[0], [1,2], [], [3]]
{- output 

[0,1,2,3]
[0,1,2,3]

-}