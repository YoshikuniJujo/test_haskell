{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module FctlImage where

import Control.Arrow
import Control.Monad.Yaftee.Pipe.Apng.Decode qualified as Decode
import Data.Word
import Data.Image.Simple qualified as Image
import Data.Image.Immutable qualified as ImageI
import Data.Vector qualified as V

data GrayI = GrayI {
	grayIWidth :: Word32, grayIHeight :: Word32,
	grayIXOffset :: Word32, grayIYOffset :: Word32,
	grayIDelayNum :: Word16, grayIDelayDen :: Word16,
	grayIDisposeOp :: Word8, grayIBlendOp :: Word8,
	grayIImage :: V.Vector Word8 }

fromFctlImageGray :: Decode.Fctl -> ImageI.Gray -> GrayI
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
	ImageI.Gray {
		ImageI.grayWidth = w',
		ImageI.grayHeight = h',
		ImageI.grayBody = bd } = g

toFctlImageGray :: GrayI -> (Decode.Fctl, ImageI.Gray)
toFctlImageGray g = (
	Decode.Fctl {
		Decode.fctlSequenceNumber = 0,
		Decode.fctlWidth = w, Decode.fctlHeight = h,
		Decode.fctlXOffset = xo, Decode.fctlYOffset = yo,
		Decode.fctlDelayNum = dn, Decode.fctlDelayDen = dd,
		Decode.fctlDisposeOp = dop, Decode.fctlBlendOp = bop },
	ImageI.Gray {
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

firstImageGray :: ImageI.Gray -> Word16 -> Word16 -> GrayI
firstImageGray ImageI.Gray {
	ImageI.grayWidth = w,
	ImageI.grayHeight = h,
	ImageI.grayBody = bd } dn dd = GrayI {
	grayIWidth = fromIntegral w, grayIHeight = fromIntegral h,
	grayIXOffset = 0, grayIYOffset = 0,
	grayIDelayNum = dn, grayIDelayDen = dd,
	grayIImage = bd }

diffToFctlImageGray ::
	ImageI.Gray -> ImageI.Gray -> Word16 -> Word16 -> Maybe GrayI
diffToFctlImageGray p c dn dd = case diffGray p c of
	Nothing -> Nothing
	Just (xo, yo, ImageI.Gray {
		ImageI.grayWidth = w,
		ImageI.grayHeight = h,
		ImageI.grayBody = bd }) -> Just GrayI {
		grayIWidth = fromIntegral w, grayIHeight = fromIntegral h,
		grayIXOffset = xo, grayIYOffset = yo,
		grayIDelayNum = dn, grayIDelayDen = dd,
		grayIDisposeOp = 0, grayIBlendOp = 0,
		grayIImage = bd }

diffGray :: ImageI.Gray -> ImageI.Gray -> Maybe (Word32, Word32, ImageI.Gray)
diffGray p c = do
	(nt, (pt, ct)) <- diffGrayTop p c
	((pb, cb), _) <- diffGrayBottom pt ct
	(nl, (pl, cl)) <- diffGrayLeft pb cb
	((_, cr), _) <- diffGrayRight pl cl
	pure (nl, nt, cr)

diffGrayTop :: ImageI.Gray -> ImageI.Gray -> Maybe (Word32, (ImageI.Gray, ImageI.Gray))
diffGrayTop p c = case ImageI.grayUnconsRow p of
	Nothing -> Nothing
	Just (hp, tp) -> case ImageI.grayUnconsRow c of
		Nothing -> Nothing
		Just (hc, tc)
			| hp == hc -> ((+ 1) `first`) <$> diffGrayTop tp tc
			| otherwise -> Just (0, (p, c))

diffGrayBottom :: ImageI.Gray -> ImageI.Gray -> Maybe ((ImageI.Gray, ImageI.Gray), Word32)
diffGrayBottom p c = case ImageI.grayUnsnocRow p of
	Nothing -> Nothing
	Just (ip, lp) -> case ImageI.grayUnsnocRow c of
		Nothing -> Nothing
		Just (ic, lc)
			| lp == lc -> ((+ 1) `second`) <$> diffGrayBottom ip ic
			| otherwise -> Just ((p, c), 0)

diffGrayLeft :: ImageI.Gray -> ImageI.Gray -> Maybe (Word32, (ImageI.Gray, ImageI.Gray))
diffGrayLeft p c = case ImageI.grayUnconsCol p of
	Nothing -> Nothing
	Just (hp, tp) -> case ImageI.grayUnconsCol c of
		Nothing -> Nothing
		Just (hc, tc)
			| hp == hc -> ((+ 1) `first`) <$> diffGrayLeft tp tc
			| otherwise -> Just (0, (p, c))

diffGrayRight :: ImageI.Gray -> ImageI.Gray -> Maybe ((ImageI.Gray, ImageI.Gray), Word32)
diffGrayRight p c = case ImageI.grayUnsnocCol p of
	Nothing -> Nothing
	Just (ip, lp) -> case ImageI.grayUnsnocCol c of
		Nothing -> Nothing
		Just (ic, lc)
			| lp == lc -> ((+ 1) `second`) <$> diffGrayRight ip ic
			| otherwise -> Just ((p, c), 0)
