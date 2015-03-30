module Ch03Exer02_07 where
    
-- | 7. Define a function that joins a list of lists together using a separator value.
-- The separator should appear between elements of the list, but should not follow the last element
intersperse :: a -> [[a]] -> [a]
intersperse _ []       = []
intersperse _ [xs]     = xs
intersperse a (xs:xss) = xs ++ [a] ++ intersperse a xss
--------------------------------------------------------------
-- | The solution's main entry point
solution :: IO ()
solution = do
    print $ intersperse ',' []
    print $ intersperse ',' ["foo"]
    print $ intersperse ',' ["foo","bar","baz","quux"]
    
-- | output
-- ""
-- "foo"
-- intersperse ',' ["foo","bar","baz","quux"]
-- "foo,bar,baz,quux"