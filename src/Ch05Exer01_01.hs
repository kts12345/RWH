-------------------------------------------------------------------------------
-- 디버깅을 위해 스페이스 대신 언더바(_)를 사용함.

kSpace = '_'

fill :: Int -> Doc -> Doc 
fill w x = hcat $ best 0 [x]
    where margin col  = Text (replicate (w - col) kSpace)
          best col (d:ds) = case d of
                Empty        -> best col ds
                Char c       -> Char c : best (col + 1) ds
                Text s       -> Text s : best (col + length s) ds
                Line         -> margin col : Line:best 0 ds
                a `Concat` b -> best col (a:b:ds)
                a `Union` b  -> nicest col (best col (a:ds))
                                           (best col (b:ds))
          best col _   = [margin col] 

          nicest col a b | (w - least) `fits2` a = a
                         | otherwise                 = b
                         where least = min w col

fits2 :: Int -> [Doc]-> Bool
w `fits2` _ | w < 0      = False
w `fits2` []             = True
w `fits2` (Char '\n':_)  = True
w `fits2` (c:cs)         = (w - 1) `fits2` cs