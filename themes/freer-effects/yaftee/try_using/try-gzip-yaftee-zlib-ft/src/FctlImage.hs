{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module FctlImage where

import Control.Arrow
import Control.Monad.Yaftee.Pipe.Apng.Decode qualified as Decode
import Data.Ratio
import Data.Word
import Data.Image.Gray qualified as ImageI
import Data.Vector qualified as V

data GrayI = GrayI {
	grayIWidth :: Word32, grayIHeight :: Word32,
	grayIXOffset :: Word32, grayIYOffset :: Word32,
	grayIDelayNum :: Word16, grayIDelayDen :: Word16,
	grayIDisposeOp :: Word8, grayIBlendOp :: Word8,
	grayIImage :: V.Vector Word8 }

fromFctlImageGray :: Decode.Fctl -> ImageI.G -> GrayI
fromFctlImageGray f g
	| w == fromIntegral w', h == fromIntegral h' =
	GrayI {
		grayIWidth = w, grayIHeight = h,
		grayIXOffset = xo, grayIYOffset = yo,
		grayIDelayNum = dn, grayIDelayDen = dd,
		grayIDisposeOp = dop, grayIBlendOp = bop,
		grayIImage = bd }
	| otherwise = error "bad"
	where
	Decode.Fctl {
		Decode.fctlWidth = w, Decode.fctlHeight = h,
		Decode.fctlXOffset = xo, Decode.fctlYOffset = yo,
		Decode.fctlDelayNum = dn, Decode.fctlDelayDen = dd,
		Decode.fctlDisposeOp = dop, Decode.fctlBlendOp = bop } = f
	ImageI.G {
		ImageI.grayWidth = w',
		ImageI.grayHeight = h',
		ImageI.grayBody = bd } = g

toFctlImageGray :: GrayI -> (Decode.Fctl, ImageI.G)
toFctlImageGray g = (
	Decode.Fctl {
		Decode.fctlSequenceNumber = 0,
		Decode.fctlWidth = w, Decode.fctlHeight = h,
		Decode.fctlXOffset = xo, Decode.fctlYOffset = yo,
		Decode.fctlDelayNum = dn, Decode.fctlDelayDen = dd,
		Decode.fctlDisposeOp = dop, Decode.fctlBlendOp = bop },
	ImageI.G {
		ImageI.grayWidth = fromIntegral w,
		ImageI.grayHeight = fromIntegral h,
		ImageI.grayBody = bd } )
	where
	GrayI {
		grayIWidth = w, grayIHeight = h,
		grayIXOffset = xo, grayIYOffset = yo,
		grayIDelayNum = dn, grayIDelayDen = dd,
		grayIDisposeOp = dop, grayIBlendOp = bop,
		grayIImage = bd } = g

fromImagesGray :: [(ImageI.G, Ratio Word16)] -> [GrayI]
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

firstImageGray :: ImageI.G -> Ratio Word16 -> GrayI
firstImageGray ImageI.G {
	ImageI.grayWidth = w,
	ImageI.grayHeight = h,
	ImageI.grayBody = bd } dly = GrayI {
	grayIWidth = fromIntegral w, grayIHeight = fromIntegral h,
	grayIXOffset = 0, grayIYOffset = 0,
	grayIDelayNum = dn, grayIDelayDen = dd,
	grayIDisposeOp = 0, grayIBlendOp = 0,
	grayIImage = bd }
	where
	dn = numerator dly
	dd = numerator dly

diffToFctlImageGray ::
	ImageI.G -> ImageI.G -> Ratio Word16 -> Maybe GrayI
diffToFctlImageGray p c dly = case diffGray p c of
	Nothing -> Nothing
	Just (xo, yo, ImageI.G {
		ImageI.grayWidth = w,
		ImageI.grayHeight = h,
		ImageI.grayBody = bd }) -> Just GrayI {
		grayIWidth = fromIntegral w, grayIHeight = fromIntegral h,
		grayIXOffset = xo, grayIYOffset = yo,
		grayIDelayNum = dn, grayIDelayDen = dd,
		grayIDisposeOp = 0, grayIBlendOp = 0,
		grayIImage = bd }
	where
	dn = numerator dly
	dd = denominator dly

diffGray :: ImageI.G -> ImageI.G -> Maybe (Word32, Word32, ImageI.G)
diffGray p c = do
	(nt, (pt, ct)) <- diffGrayTop p c
	((pb, cb), _) <- diffGrayBottom pt ct
	(nl, (pl, cl)) <- diffGrayLeft pb cb
	((_, cr), _) <- diffGrayRight pl cl
	pure (nl, nt, cr)

diffGrayTop :: ImageI.G -> ImageI.G -> Maybe (Word32, (ImageI.G, ImageI.G))
diffGrayTop p c = case ImageI.grayUnconsRow p of
	Nothing -> Nothing
	Just (hp, tp) -> case ImageI.grayUnconsRow c of
		Nothing -> Nothing
		Just (hc, tc)
			| hp == hc -> ((+ 1) `first`) <$> diffGrayTop tp tc
			| otherwise -> Just (0, (p, c))

diffGrayBottom :: ImageI.G -> ImageI.G -> Maybe ((ImageI.G, ImageI.G), Word32)
diffGrayBottom p c = case ImageI.grayUnsnocRow p of
	Nothing -> Nothing
	Just (ip, lp) -> case ImageI.grayUnsnocRow c of
		Nothing -> Nothing
		Just (ic, lc)
			| lp == lc -> ((+ 1) `second`) <$> diffGrayBottom ip ic
			| otherwise -> Just ((p, c), 0)

diffGrayLeft :: ImageI.G -> ImageI.G -> Maybe (Word32, (ImageI.G, ImageI.G))
diffGrayLeft p c = case ImageI.grayUnconsCol p of
	Nothing -> Nothing
	Just (hp, tp) -> case ImageI.grayUnconsCol c of
		Nothing -> Nothing
		Just (hc, tc)
			| hp == hc -> ((+ 1) `first`) <$> diffGrayLeft tp tc
			| otherwise -> Just (0, (p, c))

diffGrayRight :: ImageI.G -> ImageI.G -> Maybe ((ImageI.G, ImageI.G), Word32)
diffGrayRight p c = case ImageI.grayUnsnocCol p of
	Nothing -> Nothing
	Just (ip, lp) -> case ImageI.grayUnsnocCol c of
		Nothing -> Nothing
		Just (ic, lc)
			| lp == lc -> ((+ 1) `second`) <$> diffGrayRight ip ic
			| otherwise -> Just ((p, c), 0)
