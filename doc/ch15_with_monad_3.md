###  요약
이전에 살펴본 스테이트 모나드와는 달리 오직 읽기만 가능하던가(설정 파일) 또는 쓰기만 가능한(로그) 상태(변수 또는 환경)를 처리해야 되는 경우가 있다.      

이전에 학습했던 인터페이스와 구현의 분리 기조는 그대로 유지하면서 읽기 전용, 쓰기 전용을 어떻게 처리하는지에 대해 공부한다.   

인터페이스 기반 프로그래밍과 Writer 모나드를 자연스럽게 연결하기 위해 File IO 작업하는 예제를 타입 클래스를 이용해 인터페이스 기반으로 작성하는 법을 중간에 보여준다.  

Reader 모나드 사용:  역시 바로 전에 보여준 방식(Supply로 감싸고, SupplyMonad 타입 클래스로 추상화)과 같은 순서로 진행하는데  
Reader 모나드의 경우 아래와 같은 흐름을 따라 작성/사용하는 방법을 보여준다.  
  1. newtype을 이용해서  e->a 함수를 감싸기: newtype Reader e a = R { runReader :: e -> a } 
  2. 모나드화 시키기:  haskell instance Monad (Reader e) where ...  
  3. 질의 함수 만들기: ask = R id   
  4. MonadSupply 타입클래스를 이용한 인터페이스 기반 코드 작성에 적용해 보기  

Writer 모나드 사용:  타입클래스 인터페이스 기반으로 Writer 모나드를 이용한 로깅 방법을 소개한다.   이벤트 정의 외에는 역시 비슷한 흐름이다.  
  1. 이벤트를 정의한다.: data Event = Open FilePath IOMode ...  
  2. newytype 을 이용해서 Writer 모나드를 감싼다.: newtype WriterIO a = W { runW :: Writer [Event] a }   
  3. Writer 모나드의 runWriter 함수에 대응되는 runWriterIO를 만들어 사용한다.  
  4. 타입클래스를 이용해 작성한 IO 처리 로직 코드를 WriterIO에 넘겨서 로깅이 수행되게 한다.   

일반 IO 작업을 타입 클래스 기반으로 작성해 놓는 경우 Pure한 로깅 작업과 연동되기 쉽다.  일반적인 IO 작업을 타입클래스를 이용해 인터페이스 기반으로 작업하는 방식을 소개한다. 흐름은 역시 비슷하다.   
(교제에서는 이 부분의 motivation은 허용하고 싶은 action과 허용하지 않을 - 또는 어렵게 허용할 - action 을 구분짓기 이다)  
  1. newtype 으로 IO 타입 감싸기: newtype HandleIO a = HandleIO { runHandleIO :: IO a }  
  2. 모나드화 시키기: deriving (Monad)  
  3. 허용하고 싶은 action 들만 감싸기: openFile path mode = HandleIO (System.IO.openFile path mode)  
  4. 타입클래스를 이용한 인터페이스 기반 코드 작성해 보기: class Monad m => MonadHandle h m | m -> h where  

기타 참고: MonadIO 타입클래스에는 liftIO가 정의되어 있고, IO 는 MonadIO 의 인스턴스이다. liftIO를 통해 IO a 를 어떤 모나드 타입으로 쉽게 변환할 수 있어서 우리가 만든 HandleIO 를 MonadIO의 인스턴스로 하고 liftIO = HandleIO로 정의하는 순간... 아주 쉽게 함수 노출/사용이 가능해 진다.

 
  
  

