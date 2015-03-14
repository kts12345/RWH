module Ch01Exer03 where

-- | The solution's main entry porint
solution = interact wordCount
    where wordCount input = (show.length.words) input ++ "\n"