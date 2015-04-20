module Ch04Exer02_04 where
-------------------------------------------------------------
takeWhile_r :: (a -> Bool) -> [a] -> [a]
takeWhile_r _ [] = []  
takeWhile_r p (x:xs) 
    | p x       = x : takeWhile_r p xs
    | otherwise = []
-----
takeWhile_f :: (a -> Bool) -> [a] -> [a]
takeWhile_f p xs = foldr step [] xs
  where step x acc | p x       = x : acc
                   | otherwise = [] 
-------------------------------------------------------------
-- | The solution's main entry point
solution :: IO ()
solution = do
    print $ takeWhile_r even [1,3,5,2,4,6]
    print $ takeWhile_f even [1,3,5,2,4,6]
    print $ takeWhile_r even [2,4,6,7,9,11]
    print $ takeWhile_f even [2,4,6,7,9,11]

{- output 

[]
[]
[2,4,6]
[2,4,6]

-}