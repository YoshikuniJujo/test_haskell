{-# LANGUAGE BlockArguments, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Adam7 (poss) where

blockSizes :: Integral n => [(n, n)]
blockSizes = [(8, 8), (4, 8), (4, 4), (2, 4), (2, 2), (1, 2), (1, 1)]

yinit :: Integral n => [(n, n)]
yinit = [(0, 8), (0, 8), (4, 8), (0, 4), (2, 4), (0, 2), (1, 2)]

mkys :: Integral n => n -> n -> n -> [n]
mkys h s i = [s, s + i .. h - 1]

mkyss :: Integral n => Bool -> n -> [(n, (n, n))]
mkyss False h = concat $ uncurry ((((, (1, 1)) <$>) <$>) . mkys h) <$> yinit
mkyss True h = concat $ zipWith (\yi bs -> uncurry ((((, bs) <$>) <$>) . mkys h) yi) yinit blockSizes
-- mkyss True h = concat $ uncurry ((fmap (\ys -> (, (1, 1)) <$> ys)) . mkys h) <$> yinit

xinit :: Integral n => [(n, n)]
xinit = [(0, 8), (4, 8), (0, 4), (2, 4), (0, 2), (1, 2), (0, 1)]

mkxs :: Integral n => n -> (n, n) -> n -> (n, n) -> [[n]]
mkxs w (sx, ix) h (sy, iy) = replicate t [sx, sx + ix .. w - 1]
	where
	t = length $ mkys h sy iy

mkxss :: Integral n => n -> n -> [[n]]
mkxss w h = concat $ (<$> (xinit `zip` yinit)) \(xi, yi) -> mkxs w xi h yi

poss :: Integral n => n -> n -> [[(n, n)]]
poss w h = zipWith (\xs y -> (, y) <$> xs) (mkxss w h) (fst <$> mkyss False h)
