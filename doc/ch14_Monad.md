
## Chapter 14. Monad (14장. 모나드) ##
------
### ■ The Maybe monad  
* 모나드 타입 중 가장 간단.
* 결과를 만들지 못할 수도 있는 computation(계산) 을 표현.
```haskell
        instance Monad Maybe where
        
        Just x >>= k  =  k x
        Nothing >>= _ =  Nothing
    
        Just _ >> k   =  k
        Nothing >> _  =  Nothing
   
        return x      =  Just x
  
        fail _        =  Nothing
```
* (>>=) 나 (>>)로 여러 computation을 엮을(chain) 때 
  어떤 computation이 Nothing을 리턴하면  
  이후의 computaion을 평가(evaluate)하지 않음.
* 하지만 이 엮임(chain)이 완전한 short-circuted 은 아님을 유의해야 함. (c 에서 삼항 연산자 ?: 나  논리 연산자 && || 는 short-circute 임)
  * Nothing이 계속 matching 되면서 오른쪽으로 전파되는 것임.
  * Nothing 전파는 런타임 비용이 많지는 않으나 공짜도 아님.

#### Executing the Maybe monad (Maybe 모나드 수행)  
* '모나드를 수행시킨다'는 의미
  * 모나드를 평가(evaluating)하고 모나드 타입 감싸개(Wrapper)를 벗김
* 벗기기 함수 maybe 
    ```haskell
     -- ---------------------------------------
        maybe :: b -> (a -> b) -> Maybe a -> b
     -- ---------------------------------------
     --          n       f           X       y  
     -- ---------------------------------------
        maybe    n       _        Nothing  = n      

        maybe    _       f        (Just x) = f x    

     --   n:=  X 가 Nothing 일때 리턴 값. y = n
     --   f:=  X 가 Just x일 때 리턴값 계산을 위해 x에 apply할 함수.  y = f(x) 
    ```
  * Maybe 모나드의 감쌈(wrapp) 형태는 너무 간단해서 패턴 매칭으로 쉽게 벗길 수 있음.
  * 패턴 매칭과 maybe 를 각 상황에 맞게 사용하면 됨. 

#### Maybe at work, and good API design (?)
* 예제. 고객 이름을 입력 받아 청구서 발송 주소지 알아내기
```haskell
-- 타입 정의하기 
import qualified Data.Map as M

type PersonName = String
type PhoneNumber = String
type BillingAddress = String
data MobileCarrier = Honest_Bobs_Phone_Network
                   | Morrisas_Marvelous_Mobiles
                   | Petes_Plutocratic_Phones
                     deriving (Eq, Ord)

findCarrierBillingAddress :: PersonName
                          -> M.Map PersonName PhoneNumber
                          -> M.Map PhoneNumber MobileCarrier
                          -> M.Map MobileCarrier BillingAddress
                          -> Maybe BillingAddress

-- -----------------------------------------------------------
-- |ghci> :module +Data.Map
-- |ghci> :type Data.Map.lookup
-- |Data.Map.lookup :: (Ord k, Monad m) => k -> Map k a -> m a     -- 이전
-- |Data.Map.lookup ::  Ord k           => k -> Map k a -> Maybe a -- 요즘
```

```haskell
-- 풀이1. 깊은 들여쓰기를 만드는 case 다중 사용
variation1 person phoneMap carrierMap addressMap =
    case M.lookup person phoneMap of
      Nothing -> Nothing
      Just number ->
          case M.lookup number carrierMap of
            Nothing -> Nothing
            Just carrier -> M.lookup carrier addressMap
```

```haskell
-- 풀이2. do 표기법 사용
variation2 person phoneMap carrierMap addressMap = do
  number  < - M.lookup person phoneMap
  carrier < - M.lookup number carrierMap
  address < - M.lookup carrier addressMap
  return address
```

```haskell
-- 풀이2-a. 위의 코드에서 사용한 return 은
--  절차적 언어의 전형적인 모습과 유사해 보이게 해 주지만 불필요함.
variation2a person phoneMap carrierMap addressMap = do
  number  < - M.lookup person phoneMap
  carrier < - M.lookup number carrierMap
  M.lookup carrier addressMap
```
```haskell
-- 풀이3. 구현부를 한 줄에 쓸 수도 있었음. 만약 패러미터 순서가 바뀌었었다면.
-- 그래서 flip 표준 함수를 사용해서 아래와 같이 구현 가능
variation3 person phoneMap carrierMap addressMap =
    lookup phoneMap person >>= lookup carrierMap >>= lookup addressMap
  where lookup = flip M.lookup
```

### ■ The List monad  
* 계산 결과가 몇개가 될 지 미리 알 수 없는 경우에 적합
* list 의 타입 생성자는 패러미터가 1개이므로 Monad로 사용하기에 알맞음
* list가 모나드의 instance가 되기 위해 어때야 하는지 생각해 보기
```haskell
  -- 제일 쉬운 return 먼저 생각해 보기
   return :: a -> m a
   return :: a -> List a
   return :: a -> [] a
   return :: a -> [a]
   -- empty, singleton, infinite 처럼 의미를 가질 만한 return 구현은 몇 개 없음
   -- singleton 이 가장 무난함
   returnSingleton x = [x] 
```
```haskell
  -- bind 만들어 보기
   (>>=)    :: (Monad m) => m a  -> (a -> m b)  -> m b
   flip map ::             [a]      -> (a -> b)   -> [b]

   map      ::             (a -> b) -> [a]    -> [b]

   concat   :: [[a]] -> [a]

   \xs f -> concat (map f xs) :: [a] -> (a -> [a1]) -> [a1]
```
```haskell
  -- 모나드 instance 완성
    instance Monad [] where
        return x  =  [x]
        xs >>= f  =  concat (map f xs)
        xs >> f   =  concat (map (\_ -> f) xs)
        fail _    =  []
```

#### Understanding the list monad (리스트 모나드에 대한 이해)
* 리스트 comprehension 과 리스트 모나드
```haskell
-- magic
comprehensive xs ys =    [(x,y) | x < - xs, y < - ys              ]
monadic       xs ys = do {        x < - xs; y < - ys;  return (x,y) }
```
```haskell
-- 바인드 함수 표현으로 나타내어 magic 이해해 보기
blockyPlain xs ys =
    xs >>=
    \x -> ys >>=
    \y -> return (x, y)

blockyPlain_reloaded xs ys =
    concat (map (\x ->
                 concat (map (\y ->
                              return (x, y))
                         ys))
            xs)
```

#### Putting the list monad to work (리스트 모나드 활용)
```haskell
guarded :: Bool -> [a] -> [a]
guarded True  xs = xs
guarded False _  = []

multiplyTo :: Int -> [(Int, Int)]
multiplyTo n = do
    x < - [1..n]
    y < - [x..n]
        guarded (x * y == n) $
          return (x, y)
--| ghci> multiplyTo 8
--| [(1,8),(2,4)]
--| ghci> multiplyTo 100
--| [(1,100),(2,50),(4,25),(5,20),(10,10)]
--| ghci> multiplyTo 891
--| [(1,891),(3,297),(9,99),(11,81),(27,33)]
```

#### Desugaring of do blocks (do 블록 풀어 보기)
 * do 문법은 모나딕 코드를 표현하는 다른 방식
 * Desugaring : 핵심 언어 표현으로 되돌리기.
 * do 블럭의 Desugaring 규칙
   * 한 개의 액션인 경우  
        <img src="https://raw.githubusercontent.com/kts12345/RWH/master/doc/img/14_1.png" />
   
   * 두 개 이상인 경우 bind >>= 로 재귀적으로 아래와 같이 처리      
        <img src="https://raw.githubusercontent.com/kts12345/RWH/master/doc/img/14_2.png" />
   *  <- 연산자의 왼쪽 부분은 한개의 변수 혹은 보다 복잡한 일반적인 하스켈 패턴일 수 있음.  
         guard 표현식은 허용되지 않음.  
        <img src="https://raw.githubusercontent.com/kts12345/RWH/master/doc/img/14_3.png" />
     * 이 때 패턴 매칭이 실패하면 fail 이 호출됨.
   * do 블럭 안에서 let in 을 사용할 때는 in 을 생략하고 in 블록 내용을 들여쓰기를 let 에 맞춰도 됨
        <img src="https://raw.githubusercontent.com/kts12345/RWH/master/doc/img/14_4.png" />


##### Monads as a programmable semicolon (프로그래머블 세미콜론으로서의 모나드)
* 레이아웃은 하스켈의 일반적인 스타일이나 강제사항은 아님. 
* 레이아웃 대신 명시적인 {} 구조를 이용해 do 블럭 작성 가능
* 명시적 구조를 이용한 방법은 흔치 않음.
* 표현식 구분을 위해 세미콜론을 사용하는 것은 대세가 되어 감(? give rise to apt slogan).
* 모나드는 "프로그램 가능한 세미콜론"의 한 종류임. 
  * 왜냐하면 (>>) 와 (>>=)의 정의는 모나드 마다 다르게 할 수 있기 때문.

##### Why go sugar-free? (왜 풀어 쓰는가?)
* 명시적 바인드 연산자 (>>=) 사용은  
  순서 있는 action 이 아니라 컴비네이터를 이용해 함수를 엮는 것 처럼 보이게 함.  
* 모나드에 있어서 초심자라고 느낀다면  
  명시적인 바인드 연산자를 사용해야 함.
* 많은 프로그래머들은  
  실제 작동되는 방식을 반복 훈련으로 연습함으로써 동작 원리에 대한 명확한 이해를 함
* 일단 모나드에 익숙해 진 후에 적절한 스타일을 선택하면 됨. 
* flipped 버전인 (=<<) 는 어떤 스타일에서건 자주 보게 될 것임.
```hakell 
    ghci> :type (>>=)
    (>>=) :: (Monad m) => m a -> (a -> m b) -> m b
    ghci> :type (=<<)
    (=<<) :: (Monad m) => (a -> m b) -> m a -> m b 
    -- 모나닉 함수를 오른쪽에서 왼쪽 진행 스타일로 작성 시 편리함.
    -- wordCount = print . length . words =<< getContents
```
### The state monad (스테이트 모나드)
* 이전 10장에서 살펴본 Parse 회고
* 바이너리 데이터 포멧을 파싱하는 모나드 였음.
* 논리적인 두 가지 구분되는 측면이 있었음.
* Either를 이용한 에러 핸들링.
* binarystring을 스테이트로 사용
주어진 스테이트를 검사해서 어떤 결과값과 새로운 스테이트를 생성.
즉, s -> (a,s)
(리턴하는 어떤 결과 값의 타입이 a 이고 스테이트 타입이 s 라고 가정)

#### Almost a state monad (유사 스테이트 모나드)
* 먼저 유사 스테이트 모나드 작성해 봄.
* 타입 정의 부터 시작
```haskell
type SimpleState s a = s -> (a, s)
-- 스테이트를 변환하는 함수. 변환할 때 어떤 결과값을 생성.
-- 그래서 스테이트 변환 모나드라고도 불림.
```
* 모나드는 타입 패러미터가 1개 여야 하므로 아래와 같이 부분 적용(partially apply)함.
```haskell
type StringState a = SimpleState String a
```
* 우리가 만든 모나드 타입 컨스트럭터는 SimpleState s 임. s 가 생략되면 안됨.
* 모나드 작성에 필요한 또 다른 구성품 return 작성.
```haskell
    returnSt :: a -> SimpleState s a
    returnSt a = \s -> (a, s)
     -- 현재 상태와 결과값을 입력 받아 tuple 화 시키는 작업만 수행함.
     -- 아래와 같이 curry 형태로 작성해도 됨.
    returnAlt :: a -> SimpleState s a
    returnAlt a s = (a, s)
```
* 모나드 퍼즐의 마지막 단계 바인드 함수 (>>=) 작성.
```haskell
    bindSt :: (SimpleState s a) -> (a -> SimpleState s b) -> SimpleState s b
    bindSt m k = \s ->  let (a, s') = m s
                        in (k a) s'

    -- 변수명을 아래와 같이 고쳐서 더 길게 써 보면
    -- m == step
    -- k == makeStep
    -- s == oldState

    bindAlt step makeStep oldState =
        let (result, newState) = step oldState
        in  (makeStep result) newState
```
* 구현 코드 이해하기 위한 팁.
  * step  
     s -> (a, s) 타입을 가진 함수.  
     평가(evaluate)하게 되면 tuple을 얻음.  
     이 얻어진 tuple 을 s -> (a, s) 타입의 새로운 함수를 리턴하는 데 사용해야 함.  
     바인드 함수의 타입을 이 관점으로 다시 살펴 보기.  
  ```haskell
        bindAlt :: 
              (s -> (a, s))        -- step
          ->  (a -> s -> (b, s))   -- makeStep
          ->  (s -> (b, s))        -- (makeStep result) newState
  ```
#### Reading and modifying the state (스테이트 읽기 및 변경)
* 바인드 연산자는 state를 직접 조작하지는 않는다.
* 스테이트에 대해 직접 동작하는 유용한 유틸함수 필요. 
```haskell
    -- 현재 스테이트를 결과 값으로 리턴
    getSt :: SimpleState s s
    getSt = \s -> (s, s)

    -- 현재 스테이트를 무시하고, 입렵 받은 값으로 새로운 스테이트를 설정
    putSt :: s -> SimpleState s ()
    putSt s = \_ -> ((), s)
```

#### Will the real state monad please stand up? (진짜 스테이트 모나드 나와 주세요)
* 방금 전까지는 간략화를 위해서 타입 리네임(sysnonym)을 사용했었음.
* 새로운 타입으로 정의하기위해서는 wrap, unwrap 과정이 필요하고 이것은 코드 이해도를 떨어뜨림.
* Monad 인스턴스를 정의하기 위해서는 바인드와 리턴 뿐만 아니라 적절한 타입 생성자가 필요.
* 실제 정의는 다음과 같음
```haskell
    newtype State s a = State {
    runState :: s -> (a, s)
  }
```
* 타입 생성자 안에  s -> (a, s) 타입을 warp 한 것임.
* 스테이트 모나드 타입을 정의하기 위해 하스켈의 레코드 문법 사용.
  * 생성자로부터 s -> (a, s) 함수를 다시 unwrap 하기 위해서는  runState 함수를 사용. 
  *  runsStat :: State s a -> s -> (a, s). 

* return 정의는 SimpleState 에서와 거의 같음. 
```haskell
    returnState :: a -> State s a
    returnState a = State $ \s -> (a, s)
 ```
 * 스테이트 wrapper를 벗기기 위해  runState를 사용해야 하므로 바인드 연산은 좀 복잡함.
```haskell
    bindState :: State s a -> (a -> State s b) -> State s b
    bindState m k = State $ \s -> let (a, s') = runState m s
                                  in runState (k a) s'
```
* 이전 대비, wrapping과 unwrapping하는 부분이 추가됨. 
* get/put 역시 이전 코드 대비 wrapping 부분만 추가되었음. 
```haskell
    get :: State s s
    get = State $ \s -> (s, s)

    put :: s -> State s ()
    put s = State $ \_ -> ((), s)
```


