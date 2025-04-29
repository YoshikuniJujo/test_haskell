module Data.HFunctor (H(..)) where

class H h where map :: (f x -> g y) -> (x -> y) -> h f x -> h g y
