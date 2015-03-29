module Ch03Exer01_01 where

-- | Definition of List from 'Real World Haskell Textbook'
data List a = Cons a (List a)
            | Nil
             deriving (Show)

-- | 1.Write the converse of fromList for the List type: a function that takes a List a and generates a [a].
toList :: List a -> [a]
toList (Cons a xs) = a:toList xs
toList Nil         = []
--------------------------------------------------------------
-- | The solution's main entry point
solution :: IO ()
solution = do
    print $ toList $ Cons 'd' (Cons 'u' (Cons 'r' (Cons 'i' (Cons 'a' (Cons 'n' Nil))))) 
    print $ toList $ Cons (Just True) (Cons Nothing (Cons (Just False) Nil))
    
-- | output
-- "durian"
-- [Just True,Nothing,Just False] 

