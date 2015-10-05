
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
------
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



