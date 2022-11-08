{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Bounded where

data ToBounded a = NInf | ToBounded a | Inf deriving (Show, Eq, Ord)

instance Bounded (ToBounded a) where
	minBound = NInf; maxBound = Inf
