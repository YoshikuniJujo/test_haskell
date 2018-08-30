module KnownFun where

{-# NOINLINE known_fun #-}
known_fun :: a -> a
known_fun x = x

known_app :: () -> Int
known_app _ = known_fun 10
