{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module FctlImage where

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
