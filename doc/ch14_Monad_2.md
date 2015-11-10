
#### Random values in the state monad
* StdGen 을 상태로 가지는 스테이트로 모나드 
```haskell
        type RandomState a = State StdGen a
```
* 타입 리네임 : 필수는 아니나 편리함. 타이핑 줄임. 랜덤 생성기 바꾸기도 (코드 수정양이 적어서) 편함.
* 랜덤 값을 생성한다는 것 : 1)현재 생성기를 가져와서(fetch), 2)사용하고, 3)새로운 생성기로 교체해 넣는 것
```haskell
    getRandom :: Random a => RandomState a
    getRandom =
      get >>= \gen ->
      let (val, gen') = random gen in
      put gen' >>
      return val
```
* 난수 짝을 생성하기 위해 모나딕 메커니즘을 사용할 수 있음.
```haskell
    getTwoRandoms :: Random a => RandomState (a, a)
    getTwoRandoms = liftM2 (,) getRandom getRandom
```
##### 연습문제
1.  getRandom 함수를 do 표현식으로 재작성 하시오.
```haskell
    getRandom :: Random a => RandomState a
    getRandom = do 
      gen <- get
      let (val, gen') = random gen in
      put gen'
      return val
```

#### 스테이트 모나드 실행 Running the state monad
* 각 모나드별로 평가 함수가 존재한다. 
* 스테이트 모나드의 경우 여러 가지가 있었음.
  * runState : 결과값과 최종 상태 리턴.
  * evalState : 결과값만 리턴. fst runState
  * execState : 최종 상태만 리턴. snd runState
* getTwoRandom 의 구현.
```haskell
        runTwoRandoms :: IO (Int, Int)
        runTwoRandoms = do
            oldState <- getStdGen
            let (result, newState) = runState getTwoRandoms oldState
            setStdGen newState
            return result
```
#### 여러 스테이트 처리 What about a bit more state?
* 현실적으로 한개의 값만 넘기면서 프로그래밍 하기는 어려움
* 여러 상태를 관리하기 위해 data 타입으로 묶는 기법이 가능
```haskell
        data CountedRandom = CountedRandom {
             crGen :: StdGen
            ,crCount :: Int
        }

        type CRState = State CountedRandom

        getCountedRandom :: Random a => CRState a
        getCountedRandom = do
          st <- get
          let (val, gen') = random (crGen st)
          put CountedRandom { crGen = gen', crCount = crCount st + 1 }
          return val
```
  * 랜덤 시드 값과 카운트 값 두 개를 관리. 
  * 일부분만 읽거나 변경할 수 있음
  * 다음 코드는 지금까지 발생 시킨 랜덤 값의 갯수를 셈. 
```haskell
        getCount :: CRState Int
        getCount = crCount `liftM` get
```
   * 위 예제는 CountedRandom 스테이트 정의 시 레코드 문법을 사용한 이유를 보여 줌.  
     스테이트의 특정 부분을 읽기 위해 accessor 함수 사용.
     특정 부분을 변경하기 위해서는 아래와 같이 좀 수고스러움. 
```haskell
        putCount :: Int -> CRState ()
        putCount a = do
            st <- get
            put st { crCount = a }

```

   * 함수 대신 레코드 문법을 이용해서 변경했음.
   * 레코드 문법이라 함수와 같은 유연함과 우아함이 없음.
   * modify 함수 이용
      * get 과 put 을 모두 수행.
      * 스테이트 변환 함수를 입력 인자로 받음
      * 하지만 레코드 문법의 지저분함을 떨쳐 버리지 못해서 썩 만족스럽지는 않음 
```haskell
            putCountModify :: Int -> CRState ()
            putCountModify a = modify $ \st -> st { crCount = a }
```

### Monads and functors

* 카테고리 이론에 기반한 Functor와 Monad는 연관성이 높다. 
* 카테고리 이론에서 Monad는 Functor 를 이용해 만든다.
* https://wiki.haskell.org/Functor-Applicative-Monad_Proposal
  * 이론과 달리 하스켈에서 Monad 정의를 별개로 한 것은 실수로 간주되었고 2015년에 release 된 ghc 7.10 에서 수정되었다.
* 하지만 그 전에도 라이브러리 제작자들은 Monad 를 만들 때 Functor instance 가 될 수 있도록 했기 때문에 fmap 사용이 거의 모든 Monad 에 대해 사용 가능했다.
* fmap 과 liftM의 타입을 비교하면 Monad에서의 fmap 에 대한 힌트를 얻을 수 있다.
  * fmap:: (Functor f) => (a->b) -> f a -> f b
  * liftM:: (Monad m) => (a1->r) -> m a1 -> m r
  * liftM 과 동일하게 pure 함수를 monad 로 리프팅 한다.



#### 모나드에 대한 또 다른 시각
* 리스트 모나드 다시 살펴보기
```haskell
        instance Monad [] where
            return x = [x]
            xs >>= f = concat (map f xs)
        -- f:: a -> [a]
        -- (map f xs):: [[a]]
        -- (concat (map f xs)) :: [a]
```
* concat 의 generic 버전은 join 임.
* fmap, join 을 이용하면 bind 함수를 자동으로 정의할 수 있음
```haskell
        import Prelude hiding ((>>=), return)

        class Functor m => AltMonad m where
        join :: m (m a) -> m a
        return :: a -> m a

        (>>=) :: AltMonad m => m a -> (a -> m b) -> m b
        xs >>= f = join (fmap f xs)
```
* 아래와 같이 바인드를 이용해 join을 구현할 수도 있다. 어느 것이 더 좋다 할 수는 없다.  
  실제 개발환경에서는 모나드를 벗겨 내야 할 상황이 종종 생긴다.
```haskell
    join :: Monad m => m (m a) -> m a
    join x = x >>= id
    
    -- ghci> join (Just (Just 1))
    --       Just 1
    --
    -- ghci> join Nothing
    --       Nothing
    --
    -- ghci> join [[1],[2,3]]
             [1,2,3]
```

#### 모나드 규칙과 좋은 코딩 스타일
* 앞에서 functor 가 항상 지켜야 되는 규칙에 대해 살펴 봤음.
```haskell
    fmap id        ==   id 
    fmap (f . g)   ==   fmap f . fmap g
```
* 마찬가지로, 모나드가 준수해야 되는 규칙 3개가 있고,  
  컴파일러가 규칙을 강제할 수가 없기 때문에 
  규칙에 맞게 작성하는 것은 프로그래머의 책임임. 
* Monad laws 는 "내 기대를 저버리지 마" 를 형식화한 것임.
* 원칙적으로는 해당 규칙을 완전히 무시할 수 있지만,  
  보물을 지나쳐 버린 것처럼 부끄러워 해야 함.
* 규칙 1  : return x >>= f   ===   f x
  * return 은 바인드(>>=) 에 대한 왼쪽 항등원 임.
  * 바인드로 unwrap 하기만 한다면 pure value를 return 으로 감쌀 필요 없다.
```haskell
        do  y <- return x
            f y                    
        === 
        return x >>= f  
        === 
        f x
``` 
* 규칙 2 : m >>= return  ===   m
  * return 은 바인드(>>=) 에 대한 오른쪽 항등원 임.
  * 블록의 마지막 액션이 리턴하는 값을 받아서(바인드 해서) 다시 리턴할 필요가 없다. 
```haskell
        do y <- m
           return y               
        ===   
        m >>= return
        ===   
        m
```
* 규칙 3 : m >>= (\x -> f x >>= g)   ===   (m >>= f) >>= g
  * 결합법칙에 관한 규칙임
  * 규칙의 괄호 부분을 where 로 적으면 다음과 같음
```
        m >>= s
            where s x = f x >>= g
        ===
        t >>= g
            where t = m >>= f
```
  * 'extract method' 를 monad 체인에 적용할 수 있다는 뜻.   
    (http://refactoring.com/catalog/extractMethod.html)
    즉, 코드 조각을 떼어내서 의미 있는 이름을 붙여서 함수화. 
* 정리 : 이 규칙들은 더 좋은 코드 작성을 도움.
  * 처음의 두 규칙은 불필요한 return 사용을 줄일 수 있게 해 줌.
  * 세 번째 규칙은 복잡한 액션을 몇개의 간단한 액션들로 리팩토링 할 수 있게 해줌.
* 기타 : 하스켈 컴파일러는 이 규칙들이 지켜졌는지를 검증할 수 없고  
  해당 규칙을 잘 지키는 것은 프로그래머의 몫이다.