{-# LANGUAGE ScopedTypeVariables #-}

import Data.Ratio
import Data.Int
import Data.Word

safeFromIntegral :: (Integral a, Real b, Bounded b) => a -> Maybe b
safeFromIntegral n = let
	r = fromIntegral n
	mxb = maxBound `asTypeOf` r
	mnb = minBound `asTypeOf` r in
	case (n `lteq` mxb, n `gteq` mnb) of
		(True, True) -> Just r
		_ -> Nothing
{-
	| fromIntegral n <= (maxBound :: b) &&
		fromIntegral n >= (minBound :: b) = Just $ fromIntegral n
	| otherwise = Nothing
	-}

gteq, lteq :: (Integral a, Real b) => a -> b -> Bool
x `gteq` y = let
	r = toRational y
	n = numerator r
	d = denominator r in
	toInteger x * d >= n
x `lteq` y = let
	r = toRational y
	n = numerator r
	d = denominator r in
	toInteger x * d <= n
