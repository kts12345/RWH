Chapter 13. Data Structures  (13장. 자료구조)
========

Association Lists (연관 리스트):
------

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
  * 사용 예시 : 스펙 유추 
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
  
  
  
Maps (맵):
------
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
  



Functions Are Data, Too (함수도 역시 데이터다):
------
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
 ===============================
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
    (교제에서는 이미 P5 포멧 파싱하는 10장의 Tip 부분에서 간단히 정의했었음)
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

