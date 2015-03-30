module Ch03Exer02_08 where

-- | 8. Using the binary tree type that we defined earlier in this chapter, 
--      write a function that will determine the height of the tree. 
--      The height is the largest number of hops from the root to an Empty. 
--      For example, the tree Empty has height zero; 
--                   Node "x" Empty Empty has height one; 
--                   Node "x" Empty (Node "y" Empty Empty) has height two; 
--                   and so on
data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

height :: Tree a -> Int
height Empty         = 0
height (Node n l r)  = 1 + max (height l) (height r)

--------------------------------------------------------------
-- | The solution's main entry point
solution :: IO ()
solution = do
    print $ height Empty
    print $ height (Node "x" Empty Empty)
    print $ height (Node "x" Empty (Node "y" Empty Empty))

-- | output
-- 0
-- 1
-- 2
