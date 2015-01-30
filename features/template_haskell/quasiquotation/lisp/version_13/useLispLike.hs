{-# LANGUAGE QuasiQuotes #-}

import LispLike

[lisp|

(type main (IO ()))
(define main (putStrLn (++ "Hello, " "world!")))

(type greeting (-> String (IO ())))
(define (greeting n) (putStrLn (++ "Hello, " n)))

|]
