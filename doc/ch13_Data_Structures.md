
## Chapter 13. Data Structures  (13장. 자료구조)


------
### ■ Association Lists (연관 리스트)


* 무순서(unordered)지만 특정 키로 인덱싱되는 데이터를 다루어야 할 때가 많음.
 * ex) 유닉스 관리 모듈은 숫자로 된 UID를 키 값으로 그에 대응되는 사용자 이름을 얻어냄.
* 가장 많이 사용하는 다음 두 자료구조를 살펴볼 것임.
 *  association lists
    * 간단해서 사용하기 쉬움. 
    * 하스켈 표준 리스트이기 때문에  
      리스트에 대해 동작하는 모든 익숙한 함수들을 그대로 적용 가능.
 *  map
    *  Data.Map 모듈에 존재함. 
    *  데이터 양이 많은 경우에 association list 보다 월등히 뛰어난 성능을 보임.
* **association lists**
  * list[(KeyType,ValueType)]
  * 표준 스펙으로 lookup 함수 제공  
   ```haskell
     Data.List.lookup :: Eq a => a -> [(a, b)] -> Maybe b
   ```
  * 사용 예시 : 스펙 유추 d
   ```haskell
     ghci> let al = [(1, "one"), (2, "two"), (3, "three"), (4, "four")]
     ghci> lookup 1 al
     Just "one"
     ghci> lookup 5 al
     Nothing
   ```
  * 참조 구현
   ```haskell
     myLookup :: Eq a => a -> [(a, b)] -> Maybe b  
     myLookup _ [] = Nothing  
     myLookup key ((thiskey,thisval):rest) =  
        if key == thiskey  
          then Just thisval  
          else myLookup key rest  
   ```
  * 사용 예시 : 패스워드 파일 룩업
   ``` haskell
   -- 패스워드 파일 전체 내용과 UID를 입력받아서 대응되는 username 을 찾음.
   findByUID :: String -> Integer -> Maybe String
   findByUID content uid =
      let al = map parseline . lines $ content
        in lookup uid al
        
   -- 콜론 구분자로 구성된 라인에서 두 번째 필드와 세 번쩨 필드 추출.
   parseline :: String -> (Integer, String)
   parseline input =
      let fields = split ':' input
        in (read (fields !! 2), fields !! 0)

   -- 구분자와 리스트를 입력받아, 구분자 기반으로 라인을 쪼갬
   split :: Eq a => a -> [a] -> [[a]]
   split _ [] = [[]] -- 빈 리스트는 빈 리스트를 포함한 리스트로 변환
   split delim str = -- 구분자 앞부분을  "before" 로 나머지는 "remainder" 로 구분..
     let (before, remainder) = span (/= delim) str
       in  before : case remainder of
                         [] -> []                   -- 나머지가 없으면 끝내고,
                         x  -> split delim (tail x) -- 있으면 구분자 빼고 재귀 호출
                           
   ```
  
  
------  
### ■ Maps (맵)

* association lists 보다 성능이 좋음.
* 다른 언어의 hash table 과 동일 기능 제공
* 내부적으로는 균형 이진 트리 사용하여 구현됨
  * 불변 데이터를 다루는 언어에서는 해쉬 테이블보다 더 효율적인 표현임.
* 순수한 함수형 프로그래밍 우리 코드 작성에 얼마나 깊게 관여하는지에 대한 가장 극명한 예를 보여 줌.
  * 일반적으로 명확한 표현력과 성능효율을 고려하여 자료구조와 알고리즘을 선택함.
  * 동일 업무에 대해 절차형 언어에서와는 다른 자료구조와 알고리즘을 선택하는 경우가 종종 발생.
* 생성 예시  
   ```haskell
   -- Data.Map 모듈의의 함수 이름은 
   -- Prelude 의 리스트 처리 함수와 동일한 이름을 가지기 때문에 
   -- 네임스페이스 리네임 필요
   import qualified Data.Map as Map 
   
   -- association list 를 map으로 표현하기 위한  Map 생성 함수들 구현
   
    -- association list 형태의 테스트 데이터
   al = [(1, "one"), (2, "two"), (3, "three"), (4, "four")]

   -- fromList 함수 사용하여 Map 생성
   mapFromAL =  Map.fromList al  

   -- fold 를 이용하여 Map 생성
   mapFold = foldl (\map (k, v) -> Map.insert k v map) Map.empty al

   -- 빈 Map 에 insert 함수를 수동으로 호출하여 Map 생성.
   -- c.f) 생성된 내용을 print 해 보면 아래 입력 순서와는 다르게 재배열되어 출력됨
    mapManual = Map.insert 2 "two"   . 
                Map.insert 4 "four"  .
                Map.insert 1 "one"   .
                Map.insert 3 "three" $ 
                Map.empty
   ```
* Data.Map 모듈 함수들
  *  association list와 유사
  *  추가, 삭제 지원
  *  filter(필터링), map(변경), fold(누적) 지원
  *  association list와 map 간의 양방향 변환 지원
  *  c.f) 문서화가 잘 되어 있기 때문에  
     개별 함수 상세 설명 보다는  
     이번 13장에서 논의할 개념들과 관련 깊은 예제 중심으로 설명할 예정
  
------
### ■ Functions Are Data, Too (함수도 역시 데이터다)

* 함수 생성 및 조작이 쉽다는 것은 하스켈의 강력한 장점 중 하나.
* 코드 예시 : 함수를 (레코드)항목으로 가진 데이터 타입 정의 및 사용
 ```haskell
 --일반적인 사용자정의컬러 타입 정의
 data CustomColor =  CustomColor {red :: Int, green :: Int, blue :: Int}
                    deriving (Eq, Show, Read)

 --   정수값을 입력받아 (컬러값, 정수값)쌍을 리턴하는 함수를 두 번째 맴버로 가지는  타입 정의
 data FuncRec = FuncRec {name :: String, colorCalc :: Int -> (CustomColor, Int)}

 -- 컬러값과 정수값을 입력받아 정수값만 5증가한 값으로 변환.
 plus5func color x = (color, x + 5) 

 -- 자주색 값.
 purple = CustomColor 255 0 255     

 -- plus5 의 colorCalc 맴버 함수는 자주색 컬러값과  입력 인자 정수값을 5증가시킨 값을 리턴.
 plus5 = FuncRec {name = "plus5", colorCalc = plus5func purple}

 -- always0 의 colorCalc 맴버 함수는 자주색 컬러값과 정수 0을 리턴.
 always0 = FuncRec {name = "always0", colorCalc = \_ -> (purple, 0)}

 =================================
 ghci> :t plus5
 plus5 :: FuncRec
 ghci> name plus5
 "plus5"
 ghci> :t colorCalc plus5
 colorCalc plus5 :: Int -> (CustomColor, Int)
 ghci> (colorCalc plus5) 7
 (CustomColor {red = 255, green = 0, blue = 255},12)
 ghci> :t colorCalc always0
 colorCalc always0 :: Int -> (CustomColor, Int)
 ghci> (colorCalc always0) 7
 (CustomColor {red = 255, green = 0, blue = 255},0)

 ```
  * 클로저(Closure) : 위 예시 코드에서 FuncRec 에 컬러값을 저장하는 곳이 없는데 불구하고  
    맴버 함수는 자주색 컬러 값을 함수 자체에 내장하고 있다가 리턴값 생성에 사용한다.  
    (교제에서는 이미 P5 포멧 파싱하는 10장의 Tip 부분에서 간단하게 Closure 정의 했었음)
* 좀 더 고급진 예시 : 데이터를 여러 곳에서 사용하는 것을 타입 생성 함수 도움으로 쉽게 하기.
 ```haskell
 data FuncRec = FuncRec {name      :: String,                -- 함수 이름 
                         calc      :: Int -> Int,            -- 함수 구현
                         namedCalc :: Int -> (String, Int)}  -- 함수 이름도 함께 리턴하는 확장 함수

  -- 생성 함수 : 핵심 값(함수 포함)만 입력받아 각 (레코드) 필드값을 채워 넣음
 mkFuncRec :: String -> (Int -> Int) -> FuncRec
 mkFuncRec name calcfunc = FuncRec {name      = name,
                                    calc      = calcfunc,
                                    namedCalc = \x -> (name, calcfunc x)}

 plus5   = mkFuncRec "plus5"   (+ 5)
 always0 = mkFuncRec "always0" (\_ -> 0)
 
 ===============================
 ghci> :t plus5
 plus5 :: FuncRec

 ghci> name plus5
 "plus5"

 ghci> (calc plus5) 5
 10

 ghci> (namedCalc plus5) 5
 ("plus5",10)

 ghci> let plus5a = plus5 {name = "PLUS5A"} -- 특정 필드 값만 바꿈. 10장에서 다뤘던 내용. 
 ghci> name plus5a
 "PLUS5A"

 ghci> (namedCalc plus5a) 5
 ("plus5",10)  -- "PLUS5A" 로 바뀌지 않은 것 확인.
 ```

------
### ■ Extended Example(확장 예제): /etc/passwd

* 포멧이 잘 알려진 /etc/passwd 파일 읽어서 유저가 원하는 정보 출력하는 코드 작성
* Map 자료구조 2개를 동시에 제어하는 예시 코드 <sup>[각주1)](#myfootnote1)</sup>
* 동작 설명  
  * 커멘드 라인을 통해 파일명을 입력 받는다.  
  * 파일을 (lazy하게) 읽는다.
  * 메뉴를 보여주고 사용자가 선택하게 한다.
  * 사용자가 선택값에 따라 다음과 같이 보여 준다.
       * "1" -> 사용자 이름으로 찾아서 라인 보여주기
       * "2" -> UID로 찾아서 라인 보여주기
       * "3" -> 모든 파일 라인 다 보여주기
       * "4" -> 종료.  
* 주요 코드 설명
 ```haskell
  -- 라인을 나타내는 데이터. 
  data PasswdEntry = PasswdEntry {
           userName :: String, password :: String, uid :: Integer,
           gid :: Integer,  gecos :: String,  homeDir :: String,
           shell :: String
          } deriving (Eq, Ord)

  -- string 표현으로 변환 (serialize)
  instance Show PasswdEntry where
    show pe = printf "%s:%s:%d:%d:%s:%s:%s" 
                (userName pe) (password pe) (uid pe) (gid pe)
                (gecos pe) (homeDir pe) (shell pe)

  -- string 파싱 (deserialize)
  instance Read PasswdEntry where
    readsPrec _ value =
        case split ':' value of
             [f1, f2, f3, f4, f5, f6, f7] ->
                 [(PasswdEntry f1 f2 (read f3) (read f4) f5 f6 f7, [])]
             x -> error $ "Invalid number of fields in input: " ++ show x
        where        
        split :: Eq a => a -> [a] -> [[a]]        
        split _ [] = [[]]
        split delim str =
            let (before, remainder) = span (/= delim) str
                in
                before : case remainder of
                              [] -> []
                              x -> split delim (tail x)  

  -- 검색을 위한 자료 구조 정의
  type UserMap = Map.Map String PasswdEntry
  type UserMap = Map.Map String PasswdEntry

  -- 1. 패스워드 파일 컨텐츠를 입력받아 라인 구분.
  -- 2. PasswdEntry 리스트로 변환
  -- 3. 두 개의 Map 생성.
  inputToMaps :: String -> (UIDMap, UserMap)
  inputToMaps inp = (uidmap, usermap) where 
    uidmap  = Map.fromList . map (\pe -> (uid pe, pe)) $ entries
    usermap = Map.fromList . map (\pe -> (userName pe, pe)) $ entries
    entries = map read (lines inp)

  -- when(조건) do 블럭 패턴 처리.  
   Control.Moand.when :: Applicative f => Bool -> f () -> f () 
   Control.Moand.when (length args /= 1) $ do
        putStrLn "Syntax: passwdmap filename"
        exitFailure
   
 ``` 

------
### ■ Extended example: Numeric Types
* 수치 타입을 정의하여 하스켈 타입 시스템의 강력함을 보여주겠음
* 일단 사용예 먼저 살펴보 기로 함. 작성할 num.hs 의 사용 예.
 ```haskell
  ghci> :load num.hs
  ghci> 5 + 1 * 3               =>   8      -- 일반적인 'perform'
  ghci> prettyShow $ 5 + 1 * 3  => "5+(1*3)"  -- 괄호로 우선순위 나타내 줌.
  ghci> prettyShow $ 5 * 1 + 3  => "(5*1)+3"  -- 괄호로 우선순위 나타내 줌.
  ghci> prettyShow $ simplify $ 5 + 1 * 3 => "5+3" -- + 텀 레벨 표현.
  ghci> rpnShow $ 5 + 1 * 3 => "5 1 3 * +"  -- 스택 표현(Reverse Polish Notation )으로 바꿈.
  ghci> rpnShow $ simplify $ 5 + 1 * 3 => "5 3 +" -- rpn 과 simplify 의 엮기. 
  ghci> prettyShow $ 5 + (Symbol "x") * 3 =>"5+(x*3)" -- 심볼 포함한 표현식
  ghci> 5 / 2 => 2.5 -- 일반적인 'perforom'
  ghci> (units 5 "m") / (units 2 "s") => 2.5_m/s -- 단위 자동 추론
  ghci> (units 5 "m") + (units 2 "s") => *** Exception: Mis-matched units in add -- 예외 검출
  ghci> (units 5 "m") + (units 2 "m") => 7_m -- 동일 단위
  ghci> (units 5 "m") / 2 => 2.5_m -- 단위와 일반 수치 
  ghci> 10 * (units 5 "m") / (units 2 "s") => 25.0_m/s --일반 수치와 혼용되어도 단위 자동 추론
  ghci> sin (units (pi / 2) "rad") =>  1.0_1.0 -- 삼각함수에 라디안 적용. 뒤의  _1.0
  ghci> sin (units 90 "deg") => 1.0_1.0 -- 삼각함수에 일반각 적용
  ghci> (units 50 "m") * sin (units 90 "deg") -- 삼각함수와 단위 혼용 50.0_m
  ghci> ((units 50 "m") * sin (units 90 "deg")) :: Units (SymbolicManip Double)
        => 50.0*sin(((2.0*pi)*90.0)/360.0)_m -- 타입 지정
  ghci> prettyShow $ dropUnits $ (units 50 "m") * sin (units 90 "deg")
        => "50.0*sin(((2.0*pi)*90.0)/360.0)" -- 
  ghci> rpnShow $ dropUnits $ (units 50 "m") * sin (units 90 "deg")
        => "50.0 2.0 pi * 90.0 * 360.0 / sin *" -- 
  ghci> (units (Symbol "x") "m") * sin (units 90 "deg")
       => x*sin(((2.0*pi)*90.0)/360.0)_m -- 
  
 ```

------
####  ■  First Steps
* 앞에서 사용한 함수들 만들어 보기
* 먼저 ***(+):: Num a => a -> a -> a*** 를 우리가 만든 타입에 대해 동작하게 하기
  * Num 타입클래스의 인스턴스가 될 새로운 타입 정의해야 함
  * 이 새로운 타입은 심볼 표현을 저장할 수 있어야 함
  * 연산자도 저장하고, 각 연산자의 피연산자들도 저장해야 함
    그래서 tree 스러운 자료구조이어야 함
   ```haskell
    -- The "operators" that we're going to support
    data Op = Plus | Minus | Mul | Div | Pow  deriving (Eq, Show)
    {- 심볼 처리를 위한 핵심타입 -}
    data SymbolicManip a = 
          Number a           -- 5와 같은 단순한 숫자.
        | Arith Op (SymbolicManip a) (SymbolicManip a) -- 재귀 정의
          deriving (Eq, Show)

     {- SymbolicManip 은 Num 의 인스턴스.
        Num 에 대한 연산자가 SymbolicManip 에 대해 어떻게 동작해야 하는지 정의 -}
    instance Num a => Num (SymbolicManip a) where
        a + b = Arith Plus a b
        a - b = Arith Minus a b
        a * b = Arith Mul a b
        negate a      = Arith Mul (Number (-1)) a
        abs a         = error "abs is unimplemented"
        signum _      = error "signum is unimplemented"
        fromInteger i = Number (fromInteger i)
   ```
------
####  ■ Completed Code (코드 완성)

------
### ■ Taking advantage of functions as data (함수를 데이터로 간주할 때의 장점)
* 두 리스트를 단순 연결하기
  * 절차형 언어에서는 수행 비용이 싸고 쉬움
  * 함수형에서는 결과 리스트가 새로 만들어지기 때문에 비용이 비쌈.
    * 절차형 언어는 성능 효율을 위해 두 원본 리스트를 수정할 수 있으나,  
      순수성을 지키는 함수형에서는 불가능한 방법임.
* 하스켈의 연결 연산자 분석
  * 정의
   ```haskell
    (++) :: [a] -> [a] -> [a]
    (x:xs) ++ ys = x : xs ++ ys
     _     ++ ys = ys
   ```
  * 위 코드에서 알 수 있듯이 수행 시간은 첫 번째 원본 리스트에 비례함
  * a++b++c++d... 일 경우 수행 시간은 O(n^2)
  * 결합 우선순위를 바꿔서 
   ```haskell 
    infixr 5 ++ 
   ```  
    위의 사용 패턴일 때 O(n)으로 할 수 있으나 다른 패턴일 때는 여전히 성능이 나쁨.
* 부분 적용(partial application)기법을 이용한 성능 향상
  * 섹션 
   ```haskell
    ghci> :type ("a" ++)
    ("a" ++) :: [Char] -> [Char] 
   ```
  * 섹션 합성
   ```haskell
    ghci> :type ("a" ++) . ("b" ++)
    ("a" ++) . ("b" ++) :: [Char] -> [Char]
    ghci> let f = ("a" ++) . ("b" ++)
    ghci> f []
    "ab"
   ```
  * 섹션 합성은 O(1) 에 수행됨
  * 합성 시에는 실제 연결이 수행(perform)되는 것이 아니라 delay 됨
  * [] 적용을 통해 연결된 실제 값을 추출할 때 
    * *++* 연산자는 왼쪽 우선 순위이지만 합성 연산자 *.* 는 오른쪽 우선순위라 오른 쪽 리스트부터 연결 되기 때문에 O(n^2)이 아닌 O(n) 에 수행 가능.
  * 결론
    * 익숙하지 않은 자료구조이긴 했으나
    * 함수를 데이터로 다룸으로써
    * 성능 문제를 해결
    * 이런 접근 방법을 difference list 라고 함. https://en.wikipedia.org/wiki/Difference_list  참조
  * ToDo : (++), (.), 부분적용 등을 숨겨서 보기 좋게 하기
 

#### ■ Turning difference lists into a proper library(difference lists를 라이브리리화 하기)
```haskell 
 -- 모듈 인터페이스
 module DList (DList, fromList, toList, empty, 
                append, cons, dfoldr) where
 
 ----------------------------
 -- 파라미터화된 타입 DList 정의
 -- 해체자(unconstructor) unDL
 newtype DList a = DL { unDL :: [a] -> [a] }

 ----------------------------
 --리스트 함수 정의
 
 -- 리스트 (++) 에 대응되는 append
 append :: DList a -> DList a -> DList a
 append xs ys = DL (unDL xs . unDL ys)
 -- 또는 append (DL xs) (DL ys) = DL (xs . ys)

 -- 하스켈의 메인 타입인 리스트와의 컨버팅 함수 구현
 fromList :: [a] -> DList a
 fromList xs = DL (xs ++)

 toList :: DList a -> [a]
 toList (DL xs) = xs []

 -- DList value 생성의 seed 값.
 empty :: DList a
 empty = DL id

 -- 리스트 (:) 에 대응되는 cons
 cons :: a -> DList a -> DList a
 cons x (DL xs) = DL ((x:) . xs)
 infixr `cons`

 -- 리스트 foldr 에 대응되는 dfoldr
 dfoldr :: (a -> b -> b) -> b -> DList a -> b
 dfoldr f z xs = foldr f z (toList xs)

 -- 리스트 head에 대응되는 safeHead. O(n) 수행 시간 소요.
 safeHead :: DList a -> Maybe a
 safeHead xs = case toList xs of
                (y:_) -> Just y
                 _    -> Nothing

 ---------------------------------
 -- fmap 구현
 dmap :: (a -> b) -> DList a -> DList b
 dmap f = dfoldr go empty
     where go x xs = cons (f x) xs

 instance Functor DList where
     fmap = dmap
 ```

#### ■ Lists, difference lists, and monoids 
* 모노이드 monoid
  * 만족해야 되는 조건이 매우 적어서  
    * 수학에서 다루는 대부분 오브젝트는 모노이드.
    * 프로그래밍에서 다루는 대부분의 오브젝트도 역시 모노이드
      haskell의 특징 : 모노이드를 개념화 시켜서 명시적으로 다룬다  
  * 만족해야 되는 조건
    * 결합 법칙이 성립하는 이항 연산자가 존재해야 함
      ``` a * (b * c) == (a * b) * c ```
    * 항등원이 존재해야 함  
      그 항등원을 e라고 하면 모든 a 에 대해,  
      ```a * e == a 이고  e * a == a ```
* 모노이드 타입클래스
 ```haskell
  class Monoid a where
       mempty  :: a            -- the identity
       mappend :: a -> a -> a  -- associative binary operator
 ```

* 모노이드로서의 List 와 DList
 ```haskell
   instance Monoid [a] where
       mempty  = []
       mappend = (++)

   instance Monoid (DList a) where
       mempty  = empty
       mappend = append
 ```
* Tip : 두 연산자 모두 monoid 특성을 만족하는 경우 처리
 ```haskell
   -- 덧셈에 대한 모노이드
   newtype AInt = A { unA :: Int }  deriving (Show, Eq, Num)
   instance Monoid AInt where
       mempty = 0
       mappend = (+)

   -- 곱셈에 대한 모노이드
   newtype MInt = M { unM :: Int } deriving (Show, Eq, Num)
   instance Monoid MInt where
      mempty = 1
      mappend = (*)
 ```

  
#### ■ General purpose sequences (범용 시퀀스)  
* Data.Sequence
  * 동기 (Motivation) : List 와 DLIst 모두 특정 상황에서는 성능이 낮아짐
  * 여러 연산자들의 다양한 사용패턴에서 좋은 성능을 보임.
* 사용 방식
 ```haskell
   -- import qualified Data.Sequence as Seq
   -- import Data.Sequence ((><), (<|), (|>))
   -- import qualified Data.Foldable as Foldable

   -- 생성 예제 
    ghci> Seq.empty
    fromList []

    ghci> Seq.singleton 1
    fromList [1]

    ghci> let a = Seq.fromList [1,2,3]
 
   -- 삽입 예제 : 오른쪽 시퀀스에 왼쪽 원소 추가
    ghci> 1 <| Seq.singleton 2 
    fromList [1,2]
    ghci> :t (<|)
    (Data.Sequence.<|) :: a -> Seq a -> Seq a

   -- 삽입 예제 : 왼쪽 시퀀스에 오른쪽 원소 2 추가.
    Seq.singleton 1 |> 2
    fromList [1,2]
    ghci> :t (|>)
    (Data.Sequence.|>) :: Seq a -> a -> Seq a

   -- 연결 예제. O(log(m)), where m = min (left,right)
    ghci> let left = Seq.fromList [1,3,3]
    ghci> let right = Seq.fromList [7,1]
    ghci> left >< right
    fromList [1,3,3,7,1]
   
   --리스트로 변환 예제
    ghci> Foldable.toList (Seq.fromList [1,2,3])
    [1,2,3]

   -- reduce 예제
    ghci> Foldable.foldl' (+) 0 (Seq.fromList [1,2,3])
    6
 ```
* 시퀀스 대비 리스트 장점
  * 간단하고, 오버헤드가 적어서 대부분 task에 쉽게 사용하기 편리 
  * 시퀀스는 lazy 방식으로 사용하기 쉽지 않음
 
------
<a name="myfootnote1">각주1)</a>: 각주 테스트  
