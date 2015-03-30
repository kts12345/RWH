module Ch03Exer02_04 where


-- | 4. Turn a list into a palindrome, i.e. it should read the same both backwards and forwards. For example, given the list [1,2,3], your function should return [1,2,3,3,2,1]
palindrome :: [a] -> [a]
palindrome [] = []
palindrome xs = xs ++ reverse xs
--------------------------------------------------------------
-- | The solution's main entry point
solution :: IO ()
solution = do
    mapM_ (print.palindrome) samples
    where
        samples = [[1], [1,2], [1,2,3], [1,2,3,4], [1,2,3,4,5], []]
-- | output
