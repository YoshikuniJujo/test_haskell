{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lifegame.Tools where

div' :: Integral n => n -> n -> n
a `div'` b = (a - 1) `div` b + 1
