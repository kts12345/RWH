module Ch04Exer01_03 where

import System.Environment (getArgs)

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main' = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input,output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"

        -- replace "id" with the name of our function below
        myFunction xs = unlines      $
                        map safeHead $
                        map words    $
                        lines xs
              where safeHead [] = []
                    safeHead xs = head xs

--------------------------------------------------------------
-- | The solution's main entry point
solution :: IO ()
solution = do
    main'