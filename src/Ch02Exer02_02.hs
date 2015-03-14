module Ch02Exer02_02 where

lastButOne :: [a] -> a
lastButOne (a:b:xs)  = (last.init) (a:b:xs)
lastButOne _         = error "lastButOne. too short input"

-- | The solution's main entry porint
--solution :: IO ()
solution = do
   print $ test [1,2,3]
   print $ test [1,2]
   print $ test [1]
   where 
      test xs = "lastButOne " ++ show xs ++ " = " ++ (show $ lastButOne xs)

