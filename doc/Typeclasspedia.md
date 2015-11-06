### 3. Functor
* 가장 기본적이고 널려 있는 타입 클래스
* 직관1 : "container"를 표현. 
  * 그 컨테이너의 원소들에 어떤 함수던지 동일하게 적용할 수 있는 방안이 제공되어야 한다.
  * ex)리스트, 바이너리, ...
* 직관2 : 계산 컨텍스트를 표현. 
  * 일반적으로 더 유용한 직관이기는 하나 너무 일반적인 내용이라 도리어 정확히 설명하기는 어려움. 
* 그러나, 
  어느 직관에도 들어 맞지 않는 많은 Functor 인스턴스가 있기 때문에 
  정의 자체가 중요하고  
  현명한 학생이라면 정의와 예제에 집중해서 익숙해 지려 할 것이고,  
  익숙해 지면 직관은 따라오게 되 있다.

#### 3.1 정의
```haskell
  class Functor f where
     fmap :: (a -> b) -> f a -> f b
``` 
* f는 타입 컨스트럭터.

#### 3.2 Instance
* [], Maybe, Either
* ex)
```haskell
    instance Functor [] where
        fmap _ []     = []
        fmap g (x:xs) = g x : fmap g xs

    instance Functor Maybe where
        fmap _ Nothing  = Nothing
        fmap g (Just a) = Just (g a)
``` 
* Either e a
  * a 타입 또는 e 타입의 원소를 포함하는 컨테이너
  * 실패를 담을 수 있는 maybe와 유사하나 mayb는 실패 관련된 추가 정보를 담을 수 있음.
* ((,) e)
  * e타입의 주석(annotation)과 같은 부가 정보를 담을 수 있음.
  * (e,) 라고 생각하면 됨.
* ((->) e) 
  * (e->)라고 생각하면 됨.
  * e 를 입력으로 하는 함수를 뜻함.
  * ((->) e) a 즉, e->a 의 경우 e 타입의 값으로 인덱싱 되는 a 타입의 컨테이너로 봐도 됨
  * e 타입의 값들이 read-only로 가능한 계산 컨텍스트로 봐도 됨.
    * ((->) e)는 가끔 reader monad 로도 불림.
* IO
  * IO a 타입의 값의 의미 :a 타입의 값을 내 뱉는 계산인데, 그 계산 과정에서 I/O 가 발생할 수 있음.
  * IO a 타입의 m 이 x 값을 내 뱉는다면, g 라는 함수에 대해
    fmap g m 은 g x 를 내 뱉는데, m 과 동일한 I/O 를 발생시킴.
* 표준 container 라이브러리 
  * Set 을 제외하고는 Tree, Map 등이 모두 Functor임.

#### 3.3 Laws (법칙)
* 하스케 언어에서는 타입에 맞는 fmap 만 구현되면 되나,  
  수학적으로는 제약사항인 law 가 원래 있음.
* 하스켈에서도 '의미 있는' Funtor 인스턴스가 되기 위해서는   
  아래 두 law를 따라야 함.
  * fmap id = id
  * fmap (g . h) = (fmap g) . (fmap h)
* 법칙의 의미 
  * fmap g 는 컨테이너의 구조를 바꾸지 않고 원소만 바꿈.
  * fmap g 는 계산 컨텍스트를 변경하지 않고 값만 바꿈.
* 법칙 위배의 예
  * -- Evil Functor instance
    instance Functor [] where
      fmap _ [] = []
      fmap g (x:xs) = g x : g x : fmap g xs
* 법칙을 지키는 fmap 은 (Functor별로)단 하나만 존재함. (by free theorem)
* 첫번째 법칙(fmap id = id) 을 만족하는 Functor 인스턴스는 두번째 법칙도 자동으로 만족하게 됨.
  * 그 역은 성립하지 않는다.

#### 3.4 fmap 에 대한 직관
* 첫번째 직관 : 함수와 컨테이너를 입력받아 함수를 컨테이너 '내부의' 원소에 적용해서 새로운 컨테이너를 만든다.
* 두번째 직관 : 어떤 계산 컨택스트 상에서 한개의 변수에 함수를 적용하는데 해당 컨텍스트를 변경하지는 않는다.
* 리프팅 관점의 직관
  * curried 폼으로 보게 되면  
    fmap :: (a -> b) -> (f a -> f b) 으로 볼 수 있고,
    g::a->b 가 fmap 을 통해 
   (fmap g :: f a -> f b) 가 되는 것이다.

