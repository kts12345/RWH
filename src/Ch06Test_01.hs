import Data.List (intercalate )

class MyShow a where
  myShow :: a -> String
  myShowList :: [a] -> String
  -- default implementation
  myShowList xs = "[" ++ (intercalate "," $ map myShow xs) ++ "]"


instance MyShow Char where
  myShow c = [c]
  -- overriding the default
  myShowList xs = "\"" ++ xs ++ "\""

instance MyShow Int where
  myShow n = show n
  -- use the default myShowList

instance (MyShow a) => MyShow [a] where
  myShow = myShowList

main = do
  putStrLn $ myShow (1::Int)
  putStrLn $ myShow ([1,2,3] :: [Int])
  putStrLn $ myShow 'x'
  putStrLn $ myShow "xyz"

{- output

1
[1,2,3]
x
"xyz"

-}