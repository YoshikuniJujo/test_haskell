{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module IdealList where

data List a = E | NE (NonEmpty a) deriving Show

data NonEmpty a = a :- List a deriving Show
