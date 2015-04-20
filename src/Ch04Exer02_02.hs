module Ch04Exer02_02 where
-------------
import Data.List (foldl')
import Data.Char (digitToInt)
import Control.Monad (foldM)
-------------
type ErrorMessage = String
type EitherInt =  Either ErrorMessage Int
-------------
digitToInt_either :: Char -> EitherInt 
digitToInt_either c 
  | '0' <= c && c <='9' = Right (digitToInt c)
  | otherwise           = Left  ("non-digit '" ++ [c] ++"'")
-------------
asInt_either :: String -> EitherInt 
asInt_either ('-':xs) = case (asInt_either xs) of
                          Left r  -> Left r
                          Right r -> Right (-r)
asInt_either xs       = foldM step 0 xs
    where step acc x = case (digitToInt_either x) of
                          Left r  -> Left r
                          Right r -> Right (10*acc + r)
--------------------------------------------------------------
-- | The solution's main entry point
solution :: IO ()
solution = do
    print $ asInt_either ""
    print $ asInt_either "-"
    print $ asInt_either "-3"
    print $ asInt_either "314159265358979323846"
    print $ asInt_either "foo"

{- output 

Right 0
Right 0
Right (-3)
Right 564616105916946374
Left "non-digit 'f'"

-}