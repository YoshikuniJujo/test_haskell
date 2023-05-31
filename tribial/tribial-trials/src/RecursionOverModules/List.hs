{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module RecursionOverModules.List where

import RecursionOverModules.NonEmpty qualified as NE

data List a = Nil | NE (NE.NonEmpty a) deriving Show

len :: List a -> Int
len Nil = 0
len (NE ne) = NE.len ne
