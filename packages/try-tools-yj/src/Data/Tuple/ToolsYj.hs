{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Tuple.ToolsYj (uncurryDup) where

uncurryDup :: (a -> b -> c -> d) -> ((a, b), c) -> d
uncurryDup = uncurry . uncurry
