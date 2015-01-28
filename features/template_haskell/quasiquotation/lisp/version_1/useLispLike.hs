{-# LANGUAGE QuasiQuotes #-}

import LispLike

[lisp|

(setq main (putStrLn (++ "Hello, " "world!")))

|]
