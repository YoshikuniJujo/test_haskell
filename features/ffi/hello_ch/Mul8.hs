module Mul8 where

foreign export ccall mul8 :: Int -> Int

mul8 :: Int -> Int
mul8 = (* 8)
