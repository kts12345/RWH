module Ch01Exer04 where

solution = interact wordCount
    where wordCount input = (show.length) input ++ "\n"