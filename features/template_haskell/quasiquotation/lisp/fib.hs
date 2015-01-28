{-# LANGUAGE QuasiQuotes #-}

import LispLike

[lisp|

(define fib (: 0 (: 1 (zipWith + fib (tail fib)))))

|]
