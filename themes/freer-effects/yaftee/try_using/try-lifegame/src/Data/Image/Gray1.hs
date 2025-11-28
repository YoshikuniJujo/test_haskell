{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Image.Gray1 (

	G(..), generate,
	generateFromBytesM, diff, unconsRow

	) where

import Control.Arrow
import Data.Bits
import Data.Vector qualified as V
import Data.Bool
import Data.Word

data G = G { width :: Int, height :: Int, body :: V.Vector Word8 } deriving Show

rows' :: G -> [V.Vector Word8]
rows' G { width = w, body = bd } = go bd
	where
	go :: V.Vector Word8 -> [V.Vector Word8]
	go v	| V.null v = []
		| otherwise = V.take w' v : go (V.drop w' v)
	w' = (w - 1) `div` 8 + 1

generate :: Int -> Int -> (Int -> Int -> Bool) -> G
generate w h px = G {
	width = w, height = h,
	body = V.generate (w' * h) \i ->
		boolsToWord $ (<$> [0 .. 7]) \dx ->
			px' (i `mod` w' * 8 + dx) (i `div` w') }
	where
	px' x y	| x >= w = False
		| otherwise = px x y
	w' = (w - 1) `div` 8 + 1

boolsToWord :: [Bool] -> Word8
boolsToWord bls = go 0 bls'
	where
	go r [] = r
	go r (b : bs') = go (bool id (`setBit` 0) b $ r `shiftL` 1) bs'
	bls' = bls ++ replicate (8 - length bls) False

generateFromBytesM :: Monad m => Int -> Int -> m [Word8] -> m G
generateFromBytesM w h f = do
	bd <- V.unfoldrExactNM (((w - 1) `div` 8 + 1) * h)
		(\case	[] -> f >>= \case
				[] -> error "generateFromBytesM"
				(w' : ws) -> pure (w', ws)
			w'' : ws -> pure (w'', ws)) []
	pure G { width = w, height = h, body = bd }

diff :: G -> G -> Maybe (Word32, Word32, G)
diff p c = do
	(nt, (pt, ct)) <- diffTop p c
	((pb, cb), _) <- diffBottom pt ct
	(nl, (pl, cl)) <- diffLeft pb cb
	((_, cr), _) <- diffRight pl cl
	pure (nl, nt, cr)

diffTop :: G -> G -> Maybe (Word32, (G, G))
diffTop p c = case unconsRow p of
	Nothing -> Nothing
	Just (hp, tp) -> case unconsRow c of
		Nothing -> Nothing
		Just (hc, tc)
			| hp == hc -> ((+ 1) `first`) <$> diffTop tp tc
			| otherwise -> Just (0, (p, c))

diffBottom :: G -> G -> Maybe ((G, G), Word32)
diffBottom p c = do
	(ip, lp) <- unsnocRow p
	(ic, lc) <- unsnocRow c
	if lp == lc
	then ((+ 1) `second`) <$> diffBottom ip ic
	else pure ((p, c), 0)

diffLeft :: G -> G -> Maybe (Word32, (G, G))
diffLeft p c = do
	(hp, tp) <- unconsCol p
	(hc, tc) <- unconsCol c
	if hp == hc
	then ((+ 8) `first`) <$> diffLeft tp tc
	else pure (0, (p, c))

diffRight :: G -> G -> Maybe ((G, G), Word32)
diffRight p c = do
	(ip, lp) <- unsnocCol p
	(ic, lc) <- unsnocCol c
	if lp == lc
	then ((+ 8) `second`) <$> diffRight ip ic
	else pure ((p, c), 0)

unconsRow :: G -> Maybe (V.Vector Word8, G)
unconsRow G { height = h } | h < 1 = Nothing
unconsRow G { width = w, height = h, body = bd } = Just (
	V.take w' bd,
	G { width = w, height = h - 1, body = V.drop w' bd } )
	where w' = (w - 1) `div` 8 + 1

unsnocRow :: G -> Maybe (G, V.Vector Word8)
unsnocRow G { height = h } | h < 1 = Nothing
unsnocRow G { width = w, height = h, body = bd } = Just (
	G { width = w, height = h - 1, body = dropR w' bd },
	takeR w' bd )
	where w' = (w - 1) `div` 8 + 1

unconsCol :: G -> Maybe (V.Vector Word8, G)
unconsCol G { width = 0 } = Nothing
unconsCol g@G { width = w, height = h } = Just (
	V.fromList (V.head <$> rs),
	G { width = w - 8, height = h, body = V.concat (V.tail <$> rs) } )
	where rs = rows' g

unsnocCol :: G -> Maybe (G, V.Vector Word8)
unsnocCol G { width = 0 } = Nothing
unsnocCol g@G { width = w, height = h } = Just (
	G { width = w - ((w - 1) `mod` 8 + 1), height = h, body = V.concat (V.init <$> rs) },
	V.fromList (V.last <$> rs) )
	where rs = rows' g

takeR :: Int -> V.Vector a -> V.Vector a
takeR n v = V.drop (V.length v - n) v

dropR :: Int -> V.Vector a -> V.Vector a
dropR n v = V.take (V.length v - n) v
