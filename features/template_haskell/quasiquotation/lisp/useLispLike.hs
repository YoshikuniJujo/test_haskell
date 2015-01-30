{-# LANGUAGE QuasiQuotes #-}

import LispLike

[lisp|

(data (Foo a) Bar (Baz a) FooBar)

(type main (IO ()))
(define main (putStrLn (++ "Hello, " "world!")))

(type greeting (-> String (IO ())))
(define (greeting n) (putStrLn (++ "Hello, " n)))

|]
