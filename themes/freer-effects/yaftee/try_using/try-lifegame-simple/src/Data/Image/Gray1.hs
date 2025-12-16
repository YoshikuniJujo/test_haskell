{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Image.Gray1 (

	-- * DATA TYPE

	G(..),

	-- * GENERATE

	generate, generateFromBytesM,

	-- * DIFF

	diff, unconsRow

	) where

import Control.Arrow
import Data.Bits
import Data.Function
import Data.Vector qualified as V
import Data.Bool
import Data.Word

-- DATA TYPE

data G = G { width :: Int, height :: Int, body :: V.Vector Word8 } deriving Show

-- GENERATE

generate :: Int -> Int -> (Int -> Int -> Bool) -> G
generate wdt hgt p = G {
	width = wdt, height = hgt,
	body = V.generate (wdt' * hgt) \i ->
		b2w $ (<$> [0 .. 7]) \x -> p' (i `mod` wdt' * 8 + x) (i `div` wdt') }
	where
	p' x y	| x >= wdt = False | otherwise = p x y
	wdt' = (wdt - 1) `div` 8 + 1
	b2w = foldl (\w b -> bool id (`setBit` 0) b $ w `shiftL` 1) 0
		. ((++) <$> id <*> (`replicate` False) . (8 -) . length)

generateFromBytesM :: Monad m => Int -> Int -> m [Word8] -> m G
generateFromBytesM wdt hgt a = G wdt hgt
	<$> V.unfoldrExactNM (((wdt - 1) `div` 8 + 1) * hgt)
		(\case	[] -> a >>= \case
				[] -> error "generateFromBytesM"
				w : ws -> pure (w, ws)
			w : ws -> pure (w, ws)) []

-- DIFF

diff :: G -> G -> Maybe (Word32, Word32, G)
diff p c = do
	(nt, (pt, ct)) <- diffTp p c; ((pb, cb), _) <- diffBt pt ct
	(nl, (pl, cl)) <- diffLf pb cb; ((_, cr), _) <- diffRg pl cl
	pure (nl, nt, cr)

diffTp :: G -> G -> Maybe (Word32, (G, G))
diffTp p c = do
	(hp, tp) <- unconsRow p; (hc, tc) <- unconsRow c
	if hp == hc then ((+ 1) `first`) <$> diffTp tp tc else Just (0, (p, c))

unconsRow :: G -> Maybe (V.Vector Word8, G)
unconsRow G { height = h } | h < 1 = Nothing
unconsRow G { width = w, height = h, body = bd } = Just
	(V.take w' bd, G { width = w, height = h - 1, body = V.drop w' bd })
	where w' = (w - 1) `div` 8 + 1

diffBt :: G -> G -> Maybe ((G, G), Word32)
diffBt p c = do
	(ip, lp) <- unsnocRow p; (ic, lc) <- unsnocRow c
	if lp == lc then ((+ 1) `second`) <$> diffBt ip ic else pure ((p, c), 0)

unsnocRow :: G -> Maybe (G, V.Vector Word8)
unsnocRow G { height = h } | h < 1 = Nothing
unsnocRow G { width = w, height = h, body = bd } = Just
	(G { width = w, height = h - 1, body = drr w' bd }, tkr w' bd)
	where
	w' = (w - 1) `div` 8 + 1
	tkr = tdr V.drop; drr = tdr V.take
	tdr td n = td <$> subtract n . V.length <*> id

diffLf :: G -> G -> Maybe (Word32, (G, G))
diffLf p c = do
	(hp, tp) <- unconsCol p; (hc, tc) <- unconsCol c
	if hp == hc then ((+ 8) `first`) <$> diffLf tp tc else pure (0, (p, c))

unconsCol :: G -> Maybe (V.Vector Word8, G)
unconsCol G { width = 0 } = Nothing
unconsCol g@G { width = wdt, height = hgt } = let rs = rows g in Just (
	V.fromList (V.head <$> rs),
	G { width = wdt - 8, height = hgt, body = V.concat (V.tail <$> rs) } )

diffRg :: G -> G -> Maybe ((G, G), Word32)
diffRg p c = do
	(ip, lp) <- unsnocCol p; (ic, lc) <- unsnocCol c
	if lp == lc then ((+ 8) `second`) <$> diffRg ip ic else pure ((p, c), 0)

unsnocCol :: G -> Maybe (G, V.Vector Word8)
unsnocCol G { width = 0 } = Nothing
unsnocCol g@G { width = wdt, height = hgt } = let rs = rows g in Just (
	G {	width = wdt - ((wdt - 1) `mod` 8 + 1), height = hgt,
		body = V.concat (V.init <$> rs) },
	V.fromList (V.last <$> rs) )

rows :: G -> [V.Vector Word8]
rows G { width = w, body = bd } = ($ bd) $ fix \go ->
	\case v | V.null v -> [] | otherwise -> V.take w' v : go (V.drop w' v)
	where w' = (w - 1) `div` 8 + 1
