module Ch04Exer01_02 where

--------------------------------------------------------------
splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith p xs = case dropWhile p xs of
                       []  -> []
                       xs' -> let (as, bs) = break p xs'
                              in  [as] ++ splitWith p bs
--------------------------------------------------------------
-- | The solution's main entry point
solution :: IO ()
solution = do
    mapM_ (print.splitWith even) samples
    where  samples = [
                      [], 
                      [1], 
                      [2], 
                      [1,1], 
                      [2,1], 
                      [1,2],
                      [2,1], 
                      [1,2,1], 
                      [2,1,2], 
                      [1,2,1,2], 
                      [2,1,2,1], 
                      [1,2,1,2,1], 
                      [2,1,2,1,2]
                     ]
--------------------------------------------------------------
{- Output
[]
[[1]]
[]
[[1,1]]
[[1]]
[[1]]
[[1]]
[[1],[1]]
[[1]]
[[1],[1]]
[[1],[1]]
[[1],[1],[1]]
[[1],[1]]
-}