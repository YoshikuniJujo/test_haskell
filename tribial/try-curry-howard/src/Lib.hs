{-# LANGUAGE LambdaCase, EmptyCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

data Void

type Not a = a -> Void

empty :: Void -> a
empty = \case

foo :: (a, Not a) -> b
foo (x, f) = empty $ f x
