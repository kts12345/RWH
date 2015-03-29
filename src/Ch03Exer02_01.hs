module Ch03Exer02_01 where

-- | 1. Write a function that computes the number of elements in a list. To test it, ensure that it gives the same answers as the standard length function
length' :: [a] -> Int
length' []     = 0
length' (x:xs) = 1 + length' xs

--------------------------------------------------------------
-- | The solution's main entry point
solution :: IO ()
solution = do
    mapM_ test samples
    where
        samples = [[], "1", "12", "123", "1234", "12345"]
        test xs = print $ length xs == length' xs