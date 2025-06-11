{-# LANGUAGE BlockArguments, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib (calcPos, mkyss, mkxss, check) where

calcPos :: Integral n => n -> n -> n -> ((n, n), (n, n))
calcPos w h p
	| p < wh1 = ((p `mod` w1 * 8, p `div` w1 * 8), (8, 8))
	| p < wh2 = let p' = p - wh1 in ((p' `mod` w2 * 8 + 4, p' `div` w2 * 8), (4, 8))
	| p < wh3 = let p' = p - wh2 in ((p' `mod` w3 * 4, p' `div` w3 * 8 + 4), (4, 4))
	| p < wh4 = let p' = p - wh3 in ((p' `mod` w4 * 4 + 2, p' `div` w4 * 4), (2, 4))
	| p < wh5 = let p' = p - wh4 in ((p' `mod` w5 * 2, p' `div` w5 * 4 + 2), (2, 2))
	| p < wh6 = let p' = p - wh5 in ((p' `mod` w6 * 2 + 1, p' `div` w6 * 2), (1, 2))
	| p < wh7 = let p' = p - wh6 in ((p' `mod` w, p' `div` w * 2 + 1), (1, 1))
	where
	wh1 = (w `div'` 8) * (h `div'` 8)
	wh2 = (w `div'` 4) * (h `div'` 8)
	wh3 = (w `div'` 4) * (h `div'` 4)
	wh4 = (w `div'` 2) * (h `div'` 4)
	wh5 = (w `div'` 2) * (h `div'` 2)
	wh6 = w * (h `div'` 2)
	wh7 = w * h
	w1 = w `div'` 8
	w2 = w `div'` 4 `div` 2
	w3 = w `div'` 4
	w4 = w `div'` 2 `div` 2
	w5 = w `div'` 2
	w6 = w `div` 2

div' :: Integral n => n -> n -> n
a `div'` b = (a - 1) `div` b + 1

yinit :: Integral n => [(n, n)]
yinit = [(0, 8), (0, 8), (4, 8), (0, 4), (2, 4), (0, 2), (1, 2)]

mkys :: Integral n => n -> n -> n -> [n]
mkys h s i = [s, s + i .. h - 1]

mkyss :: Integral n => n -> [n]
mkyss h = concat $ uncurry (mkys h) <$> yinit

xinit :: Integral n => [(n, n)]
xinit = [(0, 8), (4, 8), (0, 4), (2, 4), (0, 2), (1, 2), (0, 1)]

mkxs :: Integral n => n -> (n, n) -> n -> (n, n) -> [[n]]
mkxs w (sx, ix) h (sy, iy) = replicate t [sx, sx + ix .. w - 1]
	where
	t = length $ mkys h sy iy

mkxss :: Integral n => n -> n -> [[n]]
mkxss w h = concat $ (<$> (xinit `zip` yinit)) \(xi, yi) -> mkxs w xi h yi

poss :: Integral n => n -> n -> [[(n, n)]]
poss w h = zipWith (\xs y -> (, y) <$> xs) (mkxss w h) (mkyss h)

check :: Integral n => n -> n -> Bool
check w h = concat (poss w h) == (fst . calcPos w h <$> [0 .. w * h - 1])
