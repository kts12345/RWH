module Ch03Exer02_03 where
    
-- | Write a function that computes the mean of a list, i.e. the sum of all elements in the list divided by its length. (You may need to use the fromIntegral function to convert the length of the list from an integer into a floating point number.)    
mean :: Floating a => [a] -> a
-- mean [] = ??
mean xs = (/) (sum xs) ((fromIntegral.length) xs)

--------------------------------------------------------------
-- | The solution's main entry point
solution :: IO ()
solution = do
    mapM_ (print.mean) samples
    where
        samples = [[1], [1,2], [1,2,3], [1,2,3,4], [1,2,3,4,5], []]
-- | output
-- 1.0
-- 1.5
-- 2.0
-- 2.5
-- 3.0
-- NaN