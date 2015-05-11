{- 

//-------------------------------------
// 테스트 스크린샷

C:\Users\kts123.NEOWIZ>ghci
GHCi, version 7.8.3: http://www.haskell.org/ghc/  :? for help
Loading package ghc-prim ... linking ... done.
Loading package integer-gmp ... linking ... done.
Loading package base ... linking ... done.
Prelude> :type (,)
(,) :: a -> b -> (a, b)
Prelude> (,) 3 4
(3,4)
Prelude> :type (,,)
(,,) :: a -> b -> c -> (a, b, c)
Prelude> (,,) 3 4 5
(3,4,5)

//-------------------------------------

// 설명1
> :type (,)
(,) :: a -> b -> (a, b)

(,) 함수의 타입은
  패러미터를 두 개 받아서 pair를 리턴하는 함수인데
   pair의 첫번째 원소 타입은 첫번째 입력 파라미터 타입과 동일하고 
   pair의 두번째 원소 타입은 두번째 입력 파라미터 타입과 동일하다.

// 설명2
> (,) 3 4
(3,4)
   입력값을 그대로 pair로 만들어 준다.

// 설명3
(,,) 도 입력 패러미터 갯수가 2개에서 3개로 늘어난 것 외에는 (,)와 동일하다.


-}