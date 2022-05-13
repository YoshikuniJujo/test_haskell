{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Data.Bool
import Data.Maybe
import Data.List

import Data.List.Length

modifyElem :: Traversable t => Int -> (a -> a) -> t a -> t a
modifyElem ii f xs = (\(i, x) -> bool x (f x) $ i == ii) <$> fromJust (tzip [0 ..] xs)

tzipWith :: Traversable t => (a -> b -> c) -> [a] -> t b -> Maybe (t c)
tzipWith f xs = sequenceA . snd . mapAccumL pair xs
	where
	pair [] _ = ([], Nothing)
	pair (x : xs') y = (xs', Just (f x y))

tzip :: Traversable t => [a] -> t b -> Maybe (t (a, b))
tzip = tzipWith (,)

sample :: LengthL 3 (LengthL 3 Int)
sample =
	(1 :. 2 :. 3 :. NilL) :.
	(4 :. 5 :. 6 :. NilL) :.
	(7 :. 8 :. 9 :. NilL) :. NilL
