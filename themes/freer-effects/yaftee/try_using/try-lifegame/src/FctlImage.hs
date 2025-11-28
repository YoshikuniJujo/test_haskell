{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module FctlImage (

	fromImagesGray, toFctlImageGray,
	GrayI

) where

import Control.Arrow
import Data.Ratio
import Data.Word
import Data.Image.Gray qualified as Gray
import Data.Vector qualified as V

import Data.Apng qualified as Decode

data GrayI = GrayI {
	grayIWidth :: Word32, grayIHeight :: Word32,
	grayIXOffset :: Word32, grayIYOffset :: Word32,
	grayIDelayNum :: Word16, grayIDelayDen :: Word16,
	grayIDisposeOp :: Word8, grayIBlendOp :: Word8,
	grayIImage :: V.Vector Word8 }

toFctlImageGray :: GrayI -> (Decode.Fctl, Gray.G)
toFctlImageGray g = (
	Decode.Fctl {
		Decode.fctlSequenceNumber = 0,
		Decode.fctlWidth = w, Decode.fctlHeight = h,
		Decode.fctlXOffset = xo, Decode.fctlYOffset = yo,
		Decode.fctlDelayNum = dn, Decode.fctlDelayDen = dd,
		Decode.fctlDisposeOp = dop, Decode.fctlBlendOp = bop },
	Gray.G {
		Gray.grayWidth = fromIntegral w,
		Gray.grayHeight = fromIntegral h,
		Gray.grayBody = bd } )
	where
	GrayI {
		grayIWidth = w, grayIHeight = h,
		grayIXOffset = xo, grayIYOffset = yo,
		grayIDelayNum = dn, grayIDelayDen = dd,
		grayIDisposeOp = dop, grayIBlendOp = bop,
		grayIImage = bd } = g

fromImagesGray :: [(Gray.G, Ratio Word16)] -> [GrayI]
fromImagesGray [] = error "no images"
fromImagesGray ida@((i0, d0) : _) =
	firstImageGray i0 d0 : go 0 ida
	where
	go _ [] = error "no images"
	go _ [_] = []
	go d ((i1, _) : ids@((i2, d2) : _)) =
		case diffToFctlImageGray i1 i2 (d + d2) of
			Nothing -> go (d + d2) ids
			Just g -> g : go 0 ids

firstImageGray :: Gray.G -> Ratio Word16 -> GrayI
firstImageGray Gray.G {
	Gray.grayWidth = w,
	Gray.grayHeight = h,
	Gray.grayBody = bd } dly = GrayI {
	grayIWidth = fromIntegral w, grayIHeight = fromIntegral h,
	grayIXOffset = 0, grayIYOffset = 0,
	grayIDelayNum = dn, grayIDelayDen = dd,
	grayIDisposeOp = 0, grayIBlendOp = 0,
	grayIImage = bd }
	where
	dn = numerator dly
	dd = numerator dly

diffToFctlImageGray ::
	Gray.G -> Gray.G -> Ratio Word16 -> Maybe GrayI
diffToFctlImageGray p c dly = case diffGray p c of
	Nothing -> Nothing
	Just (xo, yo, Gray.G {
		Gray.grayWidth = w,
		Gray.grayHeight = h,
		Gray.grayBody = bd }) -> Just GrayI {
		grayIWidth = fromIntegral w, grayIHeight = fromIntegral h,
		grayIXOffset = xo, grayIYOffset = yo,
		grayIDelayNum = dn, grayIDelayDen = dd,
		grayIDisposeOp = 0, grayIBlendOp = 0,
		grayIImage = bd }
	where
	dn = numerator dly
	dd = denominator dly

diffGray :: Gray.G -> Gray.G -> Maybe (Word32, Word32, Gray.G)
diffGray p c = do
	(nt, (pt, ct)) <- diffGrayTop p c
	((pb, cb), _) <- diffGrayBottom pt ct
	(nl, (pl, cl)) <- diffGrayLeft pb cb
	((_, cr), _) <- diffGrayRight pl cl
	pure (nl, nt, cr)

diffGrayTop :: Gray.G -> Gray.G -> Maybe (Word32, (Gray.G, Gray.G))
diffGrayTop p c = case Gray.grayUnconsRow p of
	Nothing -> Nothing
	Just (hp, tp) -> case Gray.grayUnconsRow c of
		Nothing -> Nothing
		Just (hc, tc)
			| hp == hc -> ((+ 1) `first`) <$> diffGrayTop tp tc
			| otherwise -> Just (0, (p, c))

diffGrayBottom :: Gray.G -> Gray.G -> Maybe ((Gray.G, Gray.G), Word32)
diffGrayBottom p c = case Gray.grayUnsnocRow p of
	Nothing -> Nothing
	Just (ip, lp) -> case Gray.grayUnsnocRow c of
		Nothing -> Nothing
		Just (ic, lc)
			| lp == lc -> ((+ 1) `second`) <$> diffGrayBottom ip ic
			| otherwise -> Just ((p, c), 0)

diffGrayLeft :: Gray.G -> Gray.G -> Maybe (Word32, (Gray.G, Gray.G))
diffGrayLeft p c = case Gray.grayUnconsCol p of
	Nothing -> Nothing
	Just (hp, tp) -> case Gray.grayUnconsCol c of
		Nothing -> Nothing
		Just (hc, tc)
			| hp == hc -> ((+ 1) `first`) <$> diffGrayLeft tp tc
			| otherwise -> Just (0, (p, c))

diffGrayRight :: Gray.G -> Gray.G -> Maybe ((Gray.G, Gray.G), Word32)
diffGrayRight p c = case Gray.grayUnsnocCol p of
	Nothing -> Nothing
	Just (ip, lp) -> case Gray.grayUnsnocCol c of
		Nothing -> Nothing
		Just (ic, lc)
			| lp == lc -> ((+ 1) `second`) <$> diffGrayRight ip ic
			| otherwise -> Just ((p, c), 0)
