{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Infinite where

import Prelude hiding (cycle)

import Data.List.NonEmpty hiding (cycle)

data Infinite a = a :~ Infinite a

cycle :: NonEmpty a -> Infinite a
cycle xs = ccl xs where
	ccl (y :| ys) = y :~ case ys of
		[] -> cycle xs
		(z : zs) -> ccl (z :| zs)
