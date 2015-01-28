{-# LANGUAGE QuasiQuotes #-}

import LispLike

[lisp|

(define main (putStrLn (++ "Hello, " "world!")))

(define (greeting n) (putStrLn (++ "Hello, " n)))

|]
