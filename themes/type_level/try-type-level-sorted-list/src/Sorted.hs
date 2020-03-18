{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Sorted (
	Sorted(Nil), Singleton, Insert, Merge, Map, Numbered, numbered,
	sample0, sample1, sample2, Sample0, Sample1 ) where

import Data.Proxy

import Sorted.Internal

sample0 :: Proxy (Insert () (Insert Int (Insert Double (Singleton Bool))))
sample0 = Proxy

sample1 :: Proxy (Insert Int (Insert Bool (Insert Double (Insert Int (Singleton ())))))
sample1 = Proxy

sample2 :: Proxy (Insert Bool (Insert Integer (Insert () (Singleton Int))))
sample2 = Proxy

type Sample0 = Insert () (Insert () (Insert Int (Singleton Double)))
type Sample1 = Insert Double (Insert Bool (Singleton Integer))
