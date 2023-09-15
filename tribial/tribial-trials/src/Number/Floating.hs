{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Number.Floating where

next :: RealFloat n => n -> n
next n	| x < mx = encodeFloat (x + 1) y
	| otherwise = encodeFloat 0 (y + 1)
	where
	r = floatRadix n
	ds = floatDigits n
	mx = r ^ ds - 1
	(x, y) = decodeFloat n

incs :: forall n . RealFloat n => [n]
incs = iterate next $ encodeFloat (r ^ (ds - 1)) (- (ds - a))
	where
	r = floatRadix @n undefined
	(a, _) = floatRange @n undefined
	ds = floatDigits @n undefined
