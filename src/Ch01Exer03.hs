module Ch01Exer03 where

-- | The solution's main entry point
solution = interact wordCount
    where wordCount input = (show.length.words) input ++ "\n"