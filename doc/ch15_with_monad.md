## Chapter 15. Programming with monads 모나드로 프로그래밍하기##
------
### Golfing practice: association lists
* 웹 URL 패러미터 인코딩 예제
  * URL?key1=value1&key2=value2&key3=value3 형태임
  * HTTP 표준 스펙에서 key 다음에 value가 반드시 와야 하는지를 명시하지 않아서 value가 없어도 invalid URL 처리를 하지 않음.
* association list
  * (key,value) 쌍을 저장하는 일반적인 하스켈 데이터 타입
  * 위의 예제를 하스켈로 표현하면 다음과 같음
  ```haskell
    [("key1", Just "value1"), (key2, Just "value2"), (key3, Just "value3")]
  ```
* '영화 리뷰' 데이터를 표현하기 위한 예제
```haskell
    data MovieReview = MovieReview {
        revTitle :: String
      , revUser :: String
      , revReview :: String
    }
```
* association list 로부터 MovieReview 값을 만들어 내는 첫 번째 코드
```haskell
simpleReview :: [(String, Maybe String)] -> Maybe MovieReview
simpleReview alist =
  case lookup "title" alist of
    Just (Just title@(_:_)) ->
      case lookup "user" alist of
        Just (Just user@(_:_)) ->
          case lookup "review" alist of
            Just (Just review@(_:_)) ->
                Just (MovieReview title user review)
            _ -> Nothing -- no review
        _ -> Nothing -- no user
    _ -> Nothing -- no title
```
* Maybe 모나드 do 문법으로 '계단 현상'개선한 코드
```haskell
maybeReview alist = do
    title <- lookup1 "title" alist
    user <- lookup1 "user" alist
    review <- lookup1 "review" alist
    return (MovieReview title user review)

lookup1 key alist = case lookup key alist of
                      Just (Just s@(_:_)) -> Just s
                      _ -> Nothing
```
* lift 를 이용해 중복 부분을 더 없에도록 개선한 코드
``` haskell
liftedReview alist =
    liftM3 MovieReview (lookup1 "title" alist)
                       (lookup1 "user" alist)
                       (lookup1 "review" alist)
  
    lookup1 key alist = case lookup key alist of
                      Just (Just s@(_:_)) -> Just s
                      _ -> Nothing
```
  * 여전히 중복 부분이 보이기는 하나 더 줄이기 어려울 만큼 많이 줄었다.

### Generalised lifting 리프팅 제네릭화
* liftM 의 문제점
  * liftM5 까지만 표준 라이브러리에 구현되어 있음.
* ap 함수
```haskell
    ghci> :m +Control.Monad
    ghci> :type ap
    ap :: (Monad m) => m (a -> b) -> m a -> m b
```
  * 예상 우려사항 : 패러미터가 하나 뿐인 pure 함수만 lift 가능하지 않은가?
  * 답변 : 하스켈 함수는 본질적으로 패러미터가 하나인 함수로 볼 수 있다.
    * f:: a->b->c == f::a->(b->c) 
* ap 를 이용해서 수정한 버전
```haskell
       apReview alist =
            MovieReview `liftM` lookup1 "title" alist
                           `ap` lookup1 "user" alist
                           `ap` lookup1 "review" alist

       lookup1 key alist = case lookup key alist of
                      Just (Just s@(_:_)) -> Just s
                      _ -> Nothing
``` 
* ap 체인은 계속 확장 가능
* pure 함수의 ($) 에 대응됨.
```haskell
        ($) ::                (a -> b) -> a   -> b
        ap  :: (Monad m) => m (a -> b) -> m a -> m b
```   
* 일반적으로 ap 는 liftM2 id 또는 liftM2 ($) 로 정의된다.


### Looking for alternatives 다른 경로 찾기

* 예제 : 휴대폰 번호의 하스켈 표현 
```haskell

        data Context = Home | Mobile | Business
                        deriving (Eq, Show)

        type Phone = String

        albulena = [(Home, "+355-652-55512")]

        nils = [(Mobile, "+47-922-55-512"), (Business, "+47-922-12-121"),
                (Home, "+47-925-55-121"), (Business, "+47-922-25-551")]

        twalumba = [(Business, "+260-02-55-5121")]
```
 * 예제 (계속) 집 전화 번호, 만약 없으면 휴대폰 번호를 알기를 원할 때 필요한 코드 
 ```haskell
onePersonalPhone :: [(Context, Phone)] -> Maybe Phone
onePersonalPhone ps = case lookup Home ps of
                        Nothing -> lookup Mobile ps
                        Just n -> Just n
```
  * 집전화가 여러 개인 경우를 고려한 코드
```haskell
        allBusinessPhones :: [(Context, Phone)] -> [Phone]
        allBusinessPhones ps = map snd numbers
                where numbers = case filter (contextIs Business) ps of
                                    [] -> filter (contextIs Mobile) ps
                                    ns -> ns
        contextIs a (b, _) = a == b
```
  * 두 예제 코드의 유사점: 비슷한 형태의 case of 구조
  * 두 예제 코드의 차이점: 앞의 코드는 empty인 lookup 결과를 핸들링  
                         뒤의 코드는 non-empty 인 lookup 결과를 핸들링. 
  * 작성된 코드 테스트
```haskell
        ghci> onePersonalPhone twalumba
        Nothing
        ghci> onePersonalPhone albulena
        Just "+355-652-55512"
        ghci> allBusinessPhones nils
        ["+47-922-12-121","+47-922-25-551"]
```
#### 타입 클래스 Control.Monad.MonadPlus 
* 위 예제의 case of 패턴에서 사용 가능
```haskell
class Monad m => MonadPlus m where
   mzero :: m a	                -- empty 결과를 나타냄.
   mplus :: m a -> m a -> m a    -- 두 결과를  하나의 결과로 만드는 것을 나타냄. 
```
* Maybe 와 lists 에 대한 표준 정의
```haskell
        instance MonadPlus [] where
           mzero = []
           mplus = (++)

        instance MonadPlus Maybe where
           mzero = Nothing

           Nothing `mplus` ys  = ys
           xs      `mplus` _ = xs ```
* 위 예제에 mplus 적용
```haskell
        oneBusinessPhone :: [(Context, Phone)] -> Maybe Phone
        oneBusinessPhone ps = lookup Business ps `mplus` lookup Mobile ps

        allPersonalPhones :: [(Context, Phone)] -> [Phone]
        allPersonalPhones ps = map snd $ filter (contextIs Home) ps `mplus`
                               filter (contextIs Mobile) ps
```
  * lookup 은 Maybe를 리턴하고, filter는 list를 리턴하므로 어떤 mplus 가 상황에 맞게 동작해야 하는지가 잘 결정됨.
* 흥미로운 사실 : 모든 MonadPlus 인스턴스에 유용한 mzero, mplus 정의가 가능. 
```haskell
-- lookup 에 대한 표준 정의
        lookup :: (Eq a) => a -> [(a, b)] -> Maybe b
        lookup _ []                      = Nothing
        lookup k ((x,y):xys) | x == k    = Just y
                          | otherwise = lookup k xys
-- 그.렇.다.면.
-- 모든 MonadPlus 인스턴스에 대해 lookupM 을 다음과 같이 정의 가능
        lookupM :: (MonadPlus m, Eq a) => a -> [(a, b)] -> m b
        lookupM _ []    = mzero
        lookupM k ((x,y):xys)
            | x == k    = return y `mplus` lookupM k xys
            | otherwise = lookupM k xys

```
  * 결과가 Maybe 타입인 경우 0 개 또는 1개의 결과를 받게 하고,  
    결과가 리스트인 경우 모든 결과를 받게 한다. 
    그리고 다른 모나드 플러스 인스턴스에 대해서도 적절한 의미 있는 결과 처리가 가능하다.
* 작은 함수에서는 mplus 사용의 잇점이 안보일 수 있으나, 
  모나드 타입과 독립적으로 복잡한 코드 제어에는 장점이 있다.
* 직접 사용하지 않을 수도 있으나 다른 사람의 프로젝트를 보다 보면 만날 확률이 크다.

##### The name mplus does not imply addition
* mplus 의 'plus' 를 보고 덧셈 plus 를 연상하여 그와 같은 동작을 기대해서는 안된다.  
  물론 그런 경우도 있다. 
```haskell
-- plus 처럼 동작하는 예
ghci> [1,2,3] `mplus` [4,5,6]
[1,2,3,4,5,6]
-- 전혀 그렇지 않은 예.
ghci> Just 1 `mplus` Just 2
Just 1
```
##### Rules for working with MonadPlus 모나드플러스가 지켜야 하는 규칙
* mzero 는 바인드 연산에 대한 왼쪽 항등원으로 동작해야 함. 
```haskell
        mzero >>= f == mzero
```
* 시퀀스 연산에서 오른쪽에 mzero 가 나타나면 short circuit 으로 동작해야 함.
```haskell
    v >> mzero == mzero
```
##### Failing safely with MonadPlus 모나드플러스를 이용한 실패 동작 제어
* 모나드의 fail 함수는 대부분 error 을 호출하게 구현되어 있는데 썩 좋다고 볼 수 없다. 
* 앞서 소개한  MonadPlus 의 규칙을  이용하면 fail 과  error 를 마주치지 않고 시사적으로 에외 처리 하는게 가능하다.
* Control.Monad.guard 
```haskell
        guard        :: (MonadPlus m) => Bool -> m ()
        guard True   =  return ()
        guard False  =  mzero

```
  * 간단한 사용 예시.  
    n 의 배수인 x 를 찾는 코드.  
    배수인 경우  x 리턴. 그렇지 않은 경우 mzero 리턴.
```haskell
        x `zeroMod` n = guard ((x `mod` n) == 0) >> return x
```

