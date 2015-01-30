{-# LANGUAGE QuasiQuotes #-}

import LispLike

[lisp|

(data (Foo a) Bar (Baz a) FooBar)

(deriving (data (Hoge a) Fuga (Piyo a) HogeHoge) Show Eq)

(type main (IO ()))
(define main (putStrLn (++ "Hello, " "world!")))

(type greeting (-> String (IO ())))
(define (greeting n) (putStrLn (++ "Hello, " n)))

|]
