{-# LANGUAGE QuasiQuotes #-}

import LispLike

[lisp|

(define fibs (: 0 (: 1 (zipWith + fibs (tail fibs)))))

|]
