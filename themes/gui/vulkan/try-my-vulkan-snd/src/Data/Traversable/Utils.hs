{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Traversable.Utils (modifyElem2) where

import Data.Bool
import Data.Maybe
import Data.List

modifyElem2 :: (Traversable t, Traversable t') =>
	Int -> Int -> (a -> a) -> t (t' a) -> t (t' a)
modifyElem2 i j f xss = modifyElem i (modifyElem j f) xss

modifyElem :: Traversable t => Int -> (a -> a) -> t a -> t a
modifyElem ii f xs = (\(i, x) -> bool x (f x) $ i == ii) <$> fromJust (tzip [0 ..] xs)

tzipWith :: Traversable t => (a -> b -> c) -> [a] -> t b -> Maybe (t c)
tzipWith f xs = sequenceA . snd . mapAccumL pair xs
	where
	pair [] _ = ([], Nothing)
	pair (x : xs') y = (xs', Just (f x y))

tzip :: Traversable t => [a] -> t b -> Maybe (t (a, b))
tzip = tzipWith (,)
