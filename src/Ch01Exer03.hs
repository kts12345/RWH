module Ch01Exer03 where

solution = interact wordCount
    where wordCount input = show (length (lines input)) ++ "\n"