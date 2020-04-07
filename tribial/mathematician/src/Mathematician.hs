{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Mathematician (
	Coffee, Theorem, Mathematician, coffee, mathematician, theorem ) where

data Coffee = Coffee deriving Show
data Theorem = Theorem deriving Show
type Mathematician = Coffee -> Theorem

coffee :: Mathematician -> Theorem
coffee = ($ Coffee)

mathematician :: Mathematician
mathematician Coffee =  Theorem

theorem :: Theorem
theorem = coffee mathematician
