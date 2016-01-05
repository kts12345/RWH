###Adventures in hiding the plumbing

이전에 살펴 봄: 랜덤 넘버 접근을 위해 스테이트 모나드를 이용하는 방법

그 방법의 단점: 제작자(author)가 아닌 사용자(client)가 '상태'를 살펴보고 변경할 수 있음

인간의 본성:내부를 공개하면 누군가는 원숭이를 끌고 들어옴.  
내부 접근이 되면 라이브러리 제작자가 가정한 부분을 훼손하게 되어 찾기 힘든 버그 유발될 수 있음.

사용자들이 라이브러리 내부 코드에 종속적으로 프로그래밍을 하게 되는 경우  
라이브러리 제작자는 내부 코드 수정 시 고려할 부분이 아주 많아지는 제약 사항이 발생함.

랜덤 넘버 모나드 개선: 사용자들이 get/put을 직접적으로 하지 못하게 수정.  
몇 가지 트릭이 필요하나 크게 어렵지는 않고 하스켈 프로그래머들이 자주 사용하는 기법임. 

랜덤 넘버에 국한하지 않고 좀 더 시야를 넓혀 봄.  
임의 타입의 유니크한 값들을 제공하는 모나드 구현  

#### Supply 모나드
`runSpply`. 값들의 리스트를 제공  
```haskell
        runSupply :: Supply s a -> [s] -> (a, [s])
```
* 값들의 유일성은 우리가 보장해야 함  
값들이 난수인지, 파일 이름인지 등 값 자체에는 Supply 모나는 신경 쓰지 않음 

* 모나드 내부 동작  
외부에서 값을 요청할 때 마다 `next` 로 리스트에서 값을 순서대로 꺼내 줌.  
값이 없음을 표현해야 하므로 꺼내주는 값은 Maybe로 감싸져 있음.
```haskell
        next :: Supply s (Maybe s)
```

#### 인터페이스
복잡한 내부 구현을 숨기기 위해 타입 생성자, excution함수, next 액션만 노출함
```haskell
module Supply
    (
      Supply
    , next
    , runSupply
    ) where
```

내부 구현은 굉장히 단순함: 스테이트 모나드를 newtype화 시킨 것일 뿐임.  
```haskell
    import Control.Monad.State`
    newtype Supply s a = S (State [s] a)
    -- s: 우리가 제공하려는 값의 타입
    -- a: 모나드로 만들기 위해 제공해야 되는 일반적인 타입 패러미터 
```
S 데이터 컨스트럭터를 노출하지 않았기 때문에 스테이트 모나드에 대한 get, put 사용을 외부에서는 할 수 없음.

모나드 타입 클래스의 인스턴스를 (편하게) 만들기
```haskell
    unwrapS :: Supply s a -> State [s] a
    unwrapS (S s) = s

    instance Monad (Supply s) where
        s >>= m = S (unwrapS s >>= unwrapS . m)
        return = S . return
```
편하기는 했으나, 단순히 newtype으로 래핑한 타입에 대해서는 언어 확장으로 더 편하게 할 수 있는 방법을 제공한다.  
```haskll
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
    ...
    deriving (Monad)  
    -- 이러면 된다.  Show, Eq 도 된다.
    -- 바인드(>>=)와 return 에 대해 래핑과 언레핑을 알아서 해 준다.
```
이와 같이 newtype으로 원래 타입을 감싸고, 필요한 부분만 노출하는 데 있어서   
익스텐션을 사용하면 (deriving 적어주는) 적은 수고만 들이면 된다.

#### next 와 runSupply
```haskell
    next = S $ do st <- get
                  case st of
                      [] -> return Nothing
                      (x:xs) -> do put xs
                                   return (Just x)

    runSupply (S  m) xs = runState m xs

    -- -------------------------------
    -- 몇가지 예제 실행해 보기
    ghci> :load Supply
    ghci> runSupply next [1,2,3]
    (Just 1,[2,3])

    ghci> runSupply (liftM2 (,) next next) [1,2,3]
    ((Just 1,Just 2),[3])

    ghci> runSupply (liftM2 (,) next next) [1]
    ((Just 1,Nothing),[])

    -- --------------------------------
    -- 스테이트 모나드임을 노출하지 않았는지 확인해 보기
    ghci> :browse Supply
    data Supply s a
    next :: Supply s (Maybe s)
    runSupply :: Supply s a -> [s] -> (a, [s])

    ghci> :info Supply
    data Supply s a 	-- Defined at Supply.hs:17:8-13
    instance Monad (Supply s) -- Defined at Supply.hs:17:8-13
```     

### 난수 제공 하기 Supplying random numbers
앞에서 만들었던 Supply를 난수의 소스로 사용할 때 해결해야 될 부분.  
  * (이론적인)무한 스트림 처리  
  * IO 모나드에서 StdGen 을 get 해서 사용한 후  사용한 StdGen 을 다시 "put 해주기"   
이전에 System.Random 모듈을 살펴본 바로는 두 요구사항들을 조화시키기 어려음.  
  
getStdRandom 함수는 StdGen을 get하고 다시 put 할 수 있음.
```haskell
    ghci> :type getStdRandom
    getStdRandom :: (StdGen -> (a, StdGen)) -> IO a
```
StdGen 이 난수를 제공하면 새로운 `stdGen`을 얻기 위해 `random`을 사용할 수 있고  
무한개의 난수를 얻기 위해 `randoms`를 사용할 수 있기는 하지만  
무한 리스트와 새로운 `stdGen` 얻는 것을 동시에 하려면 어떻게 해야 하는가?

#### split 함수
`RandomGen` 타입 클래스에는 split 함수가 존재함.  
하나의 난수 생성기를 입력 받아 두 개의 생성기를 리턴.  
이와 같은 둘로 쪼개기는 impure에서는 전혀 필요가 없으나 pure 세상에서는 매우 유용함.
split 를 이용해 하나는 runSupply 에 제공할 무한 랜덤 만들기에 사용하고  
다른 하나는 IO 모나드에 되돌려 줌.
```haskell
    import Supply
    import System.Random hiding (next)

    randomsIO :: Random a => IO [a]
    randomsIO =
        getStdRandom $ \g ->
            let (a, b) = split g
            in (randoms a, b)
-- -------------------------
-- 실행 예.
ghci> (fst . runSupply next) `fmap` randomsIO
Just (-730395521)
ghci> (fst . runSupply next) `fmap` randomsIO
Just (-649400879)
```
예제 관련 첨언.  
 * runSupply 함수 리턴 값 리마인드: 모나딕 액션 실행 결과와 아직 소비하지 않은 나머지 두개를 리턴
 * 무한 수열이라서 그냥 print하면 난리가 나기 때문에 fst로 앞의 것만 출력하게 했다. 

#### 또 다른 라운드. Another round of golf

`certainPattern f (a,b) = (f a, b)`  
즉, pair 의 한 원소에만 함수를 적용하고, 다른 원소는 건드리지 않음.  
너무 자주 나와서 표준 코드에 포함되었음. 

Control.Arrow 모듈의 first 와 second 실행해 보기.
``` haskell
ghci> :m +Control.Arrow
ghci> first (+3) (1,2)
(4,2)
ghci> second odd ('a',1)
('a',True)
-- 사실 이전에 JSON 처리 다룰 때 second 함수 살펴 봤었음)
```
first 함수를 사용해서 randomsIO 구현을 한 줄에 끝낼 수 있음.
```haskell
import Control.Arrow (first)

randomsIO_golfed :: Random a => IO [a]
randomsIO_golfed = getStdRandom (first randoms . split)

```

###구현에서 인터페이스를 분리하기. Separating interface from implementation
이전 섹션에서 Supply 모나드가 값을 간직하고 있으려고 스테이트 모나드를 사용한다는 그 사실을 어떻게 숨길 수 있는지를 살펴 봤다.

코드를 보다 모듈화 시키기 위한 또다른 중요한 방법으로는  
(코드가 무엇을 할 수 있는지를 정의하는)인터페이스를  
(할 수 있도록 하도록)구현으로부터 분리 하는 것이 있다.
 
System.Random 모듈에 있는 표준 난수 생성기는 매우 느리다고 알려져 있다.  
만약 우리가 randomIO 함수를 나수들을 생성하는데 사용한다면 next 액션은 효율적으로 동작하지 않을 것이다.

이 문제를 해결하기 위한 간단하면서도 효과적인 방법은 Supply 에 더 좋은 난수 소스를 제공하는 것이다.

더 좋은 난수 소스를 제공하는 아이디어는 잠시 제쳐 두고  
여러 다른 곳에서도 사용할 수 있는 다른 접근법을 고려해 보자. 
모나드를 가지고 실행할 수 있는 액션들을 그 액션들이 어떻게 동작하는를 구현한 타입클래스에서  분리할 것이다.

```haskell
-- 모든 supply 모나드가 구현해야 되는 인터페이스 정의
class (Monad m) => MonadSupply s m | m -> s where
    next :: m (Maybe s)

-- (나중에 살펴 볼)낯선 하스켈 확장을 사용하므로 일단 참아야 함.
```

#### 다중 패러미터 타입 클래스 Multi-parameter typeclasses

타입 클래스 정의 때 사용한 코드 조각 'MonadSupply s m' 을 어찌 읽어야 할까?  

괄호로 좀 더 명확히 해 보면 다음과 같음.  
  `(MonadSupply s) m`,  
해석: 주어진 모나드 타입변수 m 을 타입 클래스 `MonadSupply s` 의 인스턴스로 만들수 있는데  
'MonadSupply s` 타입 클래스는 일반적인 타입 클래스와는 달리 패러미터(s)를 가지고 있음. 

MultiParamTypeClasses 확장은 타입클래스가 두 개 이상의 패러미터를 가질 수 있도록 함

패러미터 s:  
  - Supply 타입의 s 패러미터와 동일한 목적을 제공함  
  - next 함수에서 제공해 줄 값의 타입을 표현함. 

바인드(>>=) 와 return 을 `MonadSupply` 정의에서 언급하지 않은 이유:  
  - MonadSupply 타입 클래스 문맥(context, superclass)은 이미 `ModnadSupply s` 가 모나드여야 함을 요구하고 있기 때문임.  
  - `class (OldClass) => NewClass a where ...` 일 때 OldClass를 NewClass 의 superclass라 함. 

#### 함수식 의존관계. Functional dependencies

`| m -> s` 는 functional dependency를 뜻하고 종종 `fundep` 이라고 불림.  
 - m과 s 의 관계(relation) 을 설정 함.
 - 수직 막대 `|` 를 "such that" ("즉,") 이라 읽음.  
 - 화살표 `->`를 "uniquely determines" ("바로 결정한다") 라고 읽음.  
 - `FunctionalDependencies' 프라그마를 적어줘야 사용 가능한 언어 확장임.   
 - 타입 체커를 도와 주기 위한 장치임.

타입 체커:  
하스켈의 타입 체커는 theorem prover(명제 증명기)임.  
종료하지 않는 증명을 만나게 되면 컴파일러는 포기하거나 무한 루프에 빠짐.

`MonadSupply` 정의에서 `|m->s`를 통해 타입체커에게 해주는 얘기: 
  - 타입체커가 MondadSupply s 문맥에서 m을 보게 되면  
  - s 타입은 m을 사용하기 위해 적당한, 그리고 딱 하나로 결정되는 타입일 것이다.

만약 functional dependency를 생략하게 되면 타입 체커는 에러 메시지를 내보내며 포기함.

이해를 높이기 위한 추가 예제.
```haskell
    import qualified Supply as S

    instance MonadSupply s (S.Supply s) where
        next = S.next
    -----------------------------------
    -- 타입 변수 m 을 S.supply s 로 대치했음
    -- functional dependency 로 인해 
    -- 이제 타입 체커는 S.supply s 를 어딘가에서 보게 되면 
    -- S.supply s 는 타입클래스 MonadSupply s 의 인스턴스로 
    -- 사용될 수 있다는 것을 알게 됨.
```

`S.Supply Int` 를 function dependency 없이 사용했다고 하면  
  - MonadSupply s 의 인스턴스로 선언할 수는 있지만
  - 이 인스턴스를 사용하는 코드를 만나게 되면,
  - 이 인스턴스 타입의 Int 파라미터와 MonadSupply s 의 s 패러미터가 
  - 일치해야 하는지를 타입체커는 모르기 때문에 에러를 출력할 것이다.

Functional dependency는  
 - 사용하기 까다롭고
 - 실제 코드에서는 조금만 복잡해져도 올바른 동작을 보장하기 어렵다.
 - 다행인 점은 대부분의 사용 패턴이 우리 예제와 같은 간단한 상황으로 한정된다는 것이다. 

### 라운드 끝내기. Rounding out our module

모듈 헤더의 모양세.
```haskell
    -- file: ch15/SupplyClass.hs
    {-# LANGUAGE FlexibleInstances, FunctionalDependencies,
                MultiParamTypeClasses #-}

    module SupplyClass
        (
        MonadSupply(..)
        , S.Supply
        , S.runSupply
        ) where

```
FlexibleInstances 확장: 
 - functional dependency 때문에 사용해야 함.  
 - 몇몇 환경에서 인스턴스 작성의 제약을 좀 풀어줄 필요가 있을 때 사용.
 - 상세 내용은 많이 복잡함. 

[Tip]	확장이 언제 필요한지를 알아내는 법:
일단 그냥 컴파일 해 보면 GHC가 에러 메시지에서 확장 사용을 권장함.

#### 모나드 인터페이스로 프로그래밍 하기. Programming to a monad's interface

Supply 모나드에서 두 개의 값을 꺼내어 string 포멧으로 변환해서 리턴하는 예제.
```haskell
    showTwo :: (Show s) => Supply s String
    showTwo = do
        a <- next
        b <- next
        return (show "a: " ++ show a ++ ", b: " ++ show b)
```
위 코드는 Supply 모나드 결과 타입이 Supply s String 으로 한정되는 문제가 있음.

아래와 같이 MonadSupply 인터페이스를 구현한 모든 모나드에 동작하도록   
(함수 구현을 바꾸지 않고 타입만 바꿔서 쉽게) 제네릭화 가능.
```haskell
    showTwo_class :: (Show s, Monad m, MonadSupply s m) => m String
    showTwo_class = do
        a <- next
        b <- next
        return (show "a: " ++ show a ++ ", b: " ++ show b)

```
