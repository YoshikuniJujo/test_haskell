{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Hom where

ha :: (x -> y) -> ((a -> x) -> (a -> y))
ha f = \a -> f . a
