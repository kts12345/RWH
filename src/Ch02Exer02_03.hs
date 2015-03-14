module Ch02Exer02_03 where

-- | The solution's main entry porint
solution :: IO ()
solution = do
    print $ "Main> lastButOne [1, 2]"
    print $ "1"
    print $ "Main> lastButOne [1]"
    print $ "Exception: lastButOne. too short input"
    print $ "Main> lastButOne []"
    print $ "Exception: lastButOne. too short input"