module Ch04Exer02_06 where
-------------------------------------------------------------
import Data.List (repeat, foldl')
import Data.Char (isSpace)
-------------------------------------------------------------
any_f p xs = foldl' step False xs
  where step acc x = acc || (p x)
-----------
cycle_f :: [a] -> [a]
cycle_f xs = foldr step xs (repeat xs)
  where step xs acc = xs ++ acc
     -- repeat xs = ??
-----------
data Algorithm = Begin | Mid
words_f :: String -> [String]
words_f xs = snd $ foldr step (Begin, []) xs 
  where step x (Begin, acc) | isSpace x   = (Begin, acc)
                            | otherwise   = (Mid,   [x]:acc)
        step x (Mid, a:cc)  | isSpace x   = (Begin, a:cc)
                            | otherwise   = (Mid,   (x:a):cc)
-----------
unlines_f:: [String] -> String
unlines_f xss = foldl step [] xss 
  where step acc xs = acc ++ xs ++ "\n"
-------------------------------------------------------------
-- | The solution's main entry point
solution :: IO ()
solution = do
    print $ any_f even [1,3,5] 
    print $ any_f even [1,2,3,5] 
    print $ take 12 $ cycle_f [1,2] 
    print $ take 12 $ cycle_f "abc" 
    print $ words_f " a\r\n  bc  d\r  "
    print $ words_f "a bc d"
    print $ unlines_f ["a", "bc", "d"]

{- output 

False
True
[1,2,1,2,1,2,1,2,1,2,1,2]
"abcabcabcabc"
["a","bc","d"]
["a","bc","d"]
"a\nbc\nd\n"

-}