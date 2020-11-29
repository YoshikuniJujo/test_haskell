{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Internal.Tools where

reducer :: Foldable t => (a -> b -> b) -> t a -> b -> b
reducer = flip . foldr

reducel :: Foldable t => (b -> a -> b) -> b -> t a -> b
reducel = foldl
