{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Tribial where

makePqs :: Integral n => n -> n -> n -> [(n, n)]
makePqs i p q
	| i <= p = []
	| p <= q = (p, q) : makePqs i (p + 1) 0
	| otherwise = (p, q) : makePqs i p (q + 1)
