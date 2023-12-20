{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Determinism where

type T = Double
type History a = T -> a

historyA1, historyA2, historyA3 :: History Int
historyA1 = pure 8
historyA2 t
	| t < 10 = 13
	| otherwise = 25
historyA3 = (+) <$> historyA1 <*> historyA2
