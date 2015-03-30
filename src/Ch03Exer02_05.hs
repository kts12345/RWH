module Ch03Exer02_05 where

-- | 5. Write a function that determines whether its input list is a palindromepalindrome [] = True
palindrome :: Eq a => [a] -> Bool
palindrome xs = xs == reverse xs
--------------------------------------------------------------
-- | The solution's main entry point
solution :: IO ()
solution = do
    mapM_ (print.palindrome) samples
    where
        samples = [[1], [1,2], [1,2,1], [1,2,3,4], [1,2,3,3,2,1], []]
-- | output
-- True
-- False
-- True
-- False
-- True
-- True