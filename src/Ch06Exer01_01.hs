{-

//-------------------------------------
// 테스트 스크린샷

C:\Users\kts123.NEOWIZ>ghci
GHCi, version 7.8.3: http://www.haskell.org/ghc/  :? for help
Loading package ghc-prim ... linking ... done.
Loading package integer-gmp ... linking ... done.
Loading package base ... linking ... done.

Prelude> :module + Control.Arrow

Prelude Control.Arrow> :type second
second :: Arrow a => a b c -> a (d, b) (d, c)

Prelude Control.Arrow> let f = second length

Prelude Control.Arrow> f ("abc", "abc")
("abc",3)

//--------------------------------------
// 설명1

> :type second
second :: Arrow a => a b c -> a (d, b) (d, c)

second 타입은
 Arrow a 에 대하여 
 'b 타입을 입력으로 받아서 c 타입을 리턴하는 함수'를 입력으로 받아서
 'pair 를 입력으로 받아서 pair를 리턴하는 함수' 를 리턴한다.
    단, 이때 입력 pair 의 두번째 원소 타입은 b 이고 
             출력 pair 의 두번째 원소 타입은 c 이다.

//설명2
> let f = second length
> f ("abc", "abc")
("abc",3)

 f 는 pair 를 입력 받아 두번째 원소에만 length를 적용한 pair를 리턴하는 함수일 듯 하다.
 이건 타입만 보면 알 수 없고 실행 시켜 봐서 유추해 봐서 아는 거이다.
 소스 코드나 문서를 보지 않는 이상 귀납적 유추만 가능.

-}