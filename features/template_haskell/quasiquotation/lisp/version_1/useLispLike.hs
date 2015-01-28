{-# LANGUAGE QuasiQuotes #-}

import LispLike

[lisp|

(define main (putStrLn (++ "Hello, " "world!")))

|]
