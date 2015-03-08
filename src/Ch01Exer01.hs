module Ch01Exer01 where

-- | The main entry point.
solution :: IO ()
solution = do
    print $ 5 + 8
    print $ 3 * 5 + 8
    print $ 2 + 4
    print $ (+) 2 4
    print $ sqrt 16
    print $ succ 6
    print $ succ 7
    print $ pred 8
    print $ sin (pi / 2)
    print $ truncate pi
    print $ round 3.5
    print $ round 3.4
    print $ floor 3.7
    print $ ceiling 3.3