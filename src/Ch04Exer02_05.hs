module Ch04Exer02_05 where
-------------------------------------------------------------
groupBy_f :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy_f p [] = [] 
groupBy_f p xs = accs ++ [acc] 
        where 
            (accs, acc) = foldl step ([], []) xs
            step ([], [])       x = ([], [x])
            step (accs, (a:as)) x 
                | p a x      = (accs, (a:as) ++ [x])
                | otherwise  = (accs ++ [a:as], [x])

-------------------------------------------------------------
-- | The solution's main entry point
solution :: IO ()
solution = do
    print $ groupBy_f (>) [1] 
    print $ groupBy_f (>) [4,3,2,1]
    print $ groupBy_f (>) [1,2,3,4]
    print $ groupBy_f (>) [4,1,2,3]

{- output 

[[1]]
[[4,3,2,1]]
[[1],[2],[3],[4]]
[[4,1,2,3]]

-}