module Ch04Exer01_01 where
--------------------------------------------------------------
toSafe :: ([a]->b)->[a]->Maybe b
toSafe unsafe [] = Nothing
toSafe unsafe xs = Just (unsafe xs)

safeHead :: [a] -> Maybe a
safeHead  = toSafe head

safeTail :: [a] -> Maybe [a]
safeTail = toSafe tail

safeLast :: [a] -> Maybe a
safeLast = toSafe last

safeInit :: [a] -> Maybe [a]
safeInit = toSafe init

--------------------------------------------------------------
-- | The solution's main entry point
solution :: IO ()
solution = do
    mapM_ (print.safeHead) samples
    mapM_ (print.safeTail) samples
    mapM_ (print.safeLast) samples
    mapM_ (print.safeInit) samples
    where samples = [[], [1], [1,2]]
--------------------------------------------------------------
-- | output 
-- Nothing
-- Just 1
-- Just 1
-- Nothing
-- Just []
-- Just [2]
-- Nothing
-- Just 1
-- Just 2
-- Nothing
-- Just []
-- Just [1]