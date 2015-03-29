module Ch03Exer01_02 where
import Data.Maybe

-- | Define a tree type that has only one constructor, like our Java example. Instead of the Empty constructor, use the Maybe type to refer to a node's children
data Tree a = Node a (Maybe (Tree a)) (Maybe (Tree a))
              deriving (Show)

--------------------------------------------------------------
-- | The solution's main entry point
solution :: IO ()
solution = do
    print $ Node "parent" (Just (Node "left child"  Nothing Nothing))
                          (Just (Node "right child" Nothing Nothing))