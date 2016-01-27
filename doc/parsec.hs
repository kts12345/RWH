----------------
$ghci
Prelude>
----------------
Prelude>:set prompt "> "
>
----------------
> import qualified Text.Parsec as Lib
>
----------------
> let rp parser input = Lib.parse parser "" input
> :t rp
rp :: Lib.Stream input Data.Functor.Identity.Identity t =>
     Lib.Parsec input () a -> input -> Either ParseError a
----------------
> rp (Lib.char 'A') "AB"
Right 'A'
----------------
> rp (Lib.string "A") "AB"
Right "A"
----------------
> let abP = Lib.string "AB"
> rp abP "ABCDE"
Right "AB"
----------------
> rp abP "ACDE"
Left (line 1, column 1):
unexpected "C"
expecting "AB"
----------------
> let cdP = Lib.string "CD"
> rp (abP >> cdP) "ABCDE"
Right "CD"
----------------
> let manyAbP = Lib.many abP
> rp manyAbP "ABABAB"
Right ["AB","AB","AB"] 

> rp manyAbcP "ABCABCD"
Right ["ABC","ABC"]

> rp manyAbcP "ABCABCA"
Left (line 1, column 7):
unexpected end of input
expecting "ABC"
----------------
> let eP = Lib.char 'E'
> rp eP "EAB"
Right 'E'
> rp abP "ABEABEAB"
Right "AB"
> rp (Lib.sepBy abP eP) "ABEABEAB"
Right ["AB","AB","AB"]
> rp manyAbcP 
> rp (Lib.sepBy abP eP) "ABEABEABE"
Left (line 1, column 13):
unexpected end of input
expecting "AB"
----------------
> rp (Lib.endBy abP eP) "ABEABEABE"
Right ["AB","AB","AB"]
----------------
> let (<|>) = (Lib.<|>)
> let abOrCdP = abP <|> cdP
> rp abOrCdP "ABE"
Right "AB"
> rp abOrCdP "CDE"
Right "CD"
----------------
> let acP = Lib.string "AC"
> let abOrAc = abP <|> acP
> rp abOrAcP  "ABE"
Right "AB"
> rp abOrAcP  "ACE"
Left (line 1, column 1):
unexpected "C"
expecting "AB"
----------------
> let abAndAcP = (abP >> acP)
> rp abAndAcP "ABACE"
Right "AC"
> let abAndAcP = do {t1 <- abP; t2 <- acP; return (t1++t2)}
> rp abAndAcP "ABACE"
Right "ABAC"
----------------
> let aP = Lib.string "A"
> let bP = Lib.string "B"
> let cP = Lib.string "C"
> let abOrAcP = aP >> bP <|> cP
> rp abOrAcP "ACE"
Right "C"
> let abOrAcP = do { t1 <- aP; t2 <- bP <|> cP; return (t1++t2)}
> rp abOrAcP "ACE"
Right "AC"
----------------
> let abOrAcP = Lib.try abP <|> acP
> rp abOrAcP "ACE"
Right "AC"
----------------
> let abP = Lib.try(Lib.string "AB") <|> fail "===== need AB ====="
> rp abP "ACE"
Left (line 1, column 1):
unexpected "C"
expecting "AB"
===== need AB =====
----------------
> let abP = (Lib.string "AB") Lib.<?> fail "===== need AB ====="
> rp abP "ACE"
Left (line 1, column 1):
unexpected "C"
expecting "AB"













