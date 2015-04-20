module Ch04Exer02_01 where

import Data.List (foldl')
import Data.Char (digitToInt)

asInt_fold ('-':xs) = - (asInt_fold xs)
asInt_fold xs       = foldl' step 0 xs 
    where step acc x = 10*acc + (digitToInt x)

--------------------------------------------------------------
-- | The solution's main entry point
solution :: IO ()
solution = do
    print $ asInt_fold ""
    print $ asInt_fold "-"
    print $ asInt_fold "-3"
    print $ asInt_fold "314159265358979323846"
    print $ asInt_fold "foo"

{- output 

0
0
-3
564616105916946374
*** Exception: Char.digitToInt: not a digit 'o'

-}