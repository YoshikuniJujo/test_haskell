{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.List.Length (
	-- * LENGTHED LIST LEFT
	LengthL, RangeL(NilL, (:.)), AddL, (++.),
	-- * LENGTHED LIST RIGHT
	LengthR, RangeR(NilR, (:+)), AddR, (+++),
	-- * LEFT TO RIGHT
	LeftToRight, (++.+), leftToRight,
	-- * RIGHT TO LEFT
	RightToLeft, (++..), rightToLeft
	) where

import Data.List.Range

type LengthL n = RangeL n n

type LengthR n = RangeR n n
