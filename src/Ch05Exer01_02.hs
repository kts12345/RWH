-----------------------------------------------------------------
indent :: Int -> Doc
indent i   =   Text (replicate i ' ')
-----------------------------------------------------------------
-- 요구사항에 충실한 풀이
nest:: Int -> Doc -> Doc 
nest w x = hcat $ best 0 [x]
    where best i (d:ds) = case d of
            Empty        ->  best i ds
            Char '{'     ->  d : best (i+w) ds
            Char '['     ->  d : best (i+w) ds
            Char '}'     ->  d : best (i-w) ds
            Char ']'     ->  d : best (i-w) ds
            Line         ->  case head ds of
                               Char '}' -> d : indent (i-w) : best i ds
                               Char ']' -> d : indent (i-w) : best i ds
                               _        -> d : indent i     : best i ds 
            a `Concat` b ->  best i (a:b:ds)
            a `Union`  b ->  best i (b:ds) 
            _            ->  d : best i ds 
          best _  [] = [] 

-----------------------------------------------------------------
-- 더 이쁘게 찍히는 풀이.

nest':: Int -> Doc -> Doc 
nest' w x = hcat $ best [0] 0 [x]
    where best (i:is) col (d:ds) = case d of
            Empty        -> best (i:is) col ds
            Char '{'     -> d : indent (w-1) : best (col+w:i:is) (col+w) ds
            Char '['     -> d : indent (w-1) : best (col+w:i:is) (col+w) ds
            Char '}'     -> d : best is (col+1) ds
            Char ']'     -> d : best is (col+1) ds
            Char c       -> d : best (i:is) (col+1) ds
            Text s       -> d : best (i:is) (col+length s) ds
            Line         -> case head ds of
                              Char '}' -> d : indent (i-w) : best (i:is) i ds
                              Char ']' -> d : indent (i-w) : best (i:is) i ds
                              _        -> d : indent i     : best (i:is) i ds
            a `Concat` b -> best (i:is) col (a:b:ds)
            a `Union` b  -> best (i:is) col (b:ds) 
          best _ _ []  = [] 

-----------------------------------------------------------------
-- 더 더 이쁘게 찍히는 풀이.

nest'' :: Int -> Doc -> Doc 
nest'' w x = hcat $ best [0] 0 [x]
    where best (i:is) col (d:ds) = case d of
            Empty        -> best (i:is) col ds
            Char '{'     -> d : Line : indent (col+w) : best (col+w:i:is) (col+w) ds
            Char '['     -> d : Line : indent (col+w) : best (col+w:i:is) (col+w) ds
            Char '}'     -> d : best is (col+1) ds
            Char ']'     -> d : best is (col+1) ds
            Char c       -> d : best (i:is) (col+1) ds
            Text s       -> d : best (i:is) (col+length s) ds
            Line         -> case head ds of
                              Char '}' -> d : indent (i-w) : best (i:is) i ds
                              Char ']' -> d : indent (i-w) : best (i:is) i ds
                              _        -> d : indent i     : best (i:is) i ds 
            a `Concat` b -> best (i:is) col (a:b:ds)
            a `Union` b  -> best (i:is) col (b:ds) 
          best _ _ []  = [] 