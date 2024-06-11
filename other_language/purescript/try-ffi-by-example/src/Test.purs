module Test where

import Prelude

gcd' :: Int -> Int -> Int
gcd' 0 m = m
gcd' n 0 = n
gcd' n m
        | n > m = gcd' (n - m) m
        | otherwise = gcd' (m - n) n

foo :: { bar :: String } -> String
foo { bar : b } = b

hoge :: forall r . { piyo :: String | r } -> String
hoge { piyo : p } = p
