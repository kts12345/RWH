module Ch03Exer02_06 where
import Data.List

-- | 6. Create a function that sorts a list of lists based on the length of each sublist. 
-- (You may want to look at the sortBy function from the Data.List module
-- sortLists :: [[a]] -> [[a]]
sortLists xss = sortBy cmp xss
    where cmp a b = compare (length a) (length b)
--------------------------------------------------------------
-- | The solution's main entry point
solution :: IO ()
solution = do
    print $ sortLists samples
    where
        samples = [[1], [1,2], [1,2,1], [1,2,3,4], [1,2,3,3,2,1], []]
-- | output
-- [[],[1],[1,2],[1,2,1],[1,2,3,4],[1,2,3,3,2,1]]