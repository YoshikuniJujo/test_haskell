{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Ord.ToolsYj (clamp, Min, Max) where

clamp :: Ord a => Min a -> Max a -> a -> a
clamp mn mx x | x < mn = mn | x < mx = x | otherwise = mx

type Min a = a; type Max a = a
