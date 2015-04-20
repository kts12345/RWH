-- module Ch04Exer01_04 where

import System.Environment (getArgs)
import Data.List

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

transWords = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input,output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"
        myFunction :: String->String
        myFunction xs = unlines   $
                        transpose $
                        normalize $
                        lines xs
              where normalize xss = [take maxLen $ xs ++ (repeat ' ') | xs<-xss] 
                      where maxLen = maximum $ map length xss
--------------------------------------------------------------
-- | The solution's main entry point
main :: IO ()
main = transWords 