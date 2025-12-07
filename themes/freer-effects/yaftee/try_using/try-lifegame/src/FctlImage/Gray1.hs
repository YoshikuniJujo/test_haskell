{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module FctlImage.Gray1 (
	fromImages, toFctlImage, firstImage, G
	) where

import Data.Vector qualified as V
import Data.Ratio
import Data.Word
import Data.Apng qualified as Decode

import Data.Image.Gray1 qualified as Gray1

data G = G {
	width :: Word32, height :: Word32,
	xOffset :: Word32, yOffset :: Word32,
	delayNum :: Word16, delayDen :: Word16,
	disposeOp :: Word8, blendOp :: Word8,
	image :: V.Vector Word8 }
	deriving Show

toFctlImage :: G -> (Decode.Fctl, Gray1.G)
toFctlImage g = (
	Decode.Fctl {
		Decode.fctlWidth = w, Decode.fctlHeight = h,
		Decode.fctlXOffset = xo, Decode.fctlYOffset = yo,
		Decode.fctlDelayNum = dn, Decode.fctlDelayDen = dd,
		Decode.fctlDisposeOp = dop, Decode.fctlBlendOp = bop },
	Gray1.G {
		Gray1.width = fromIntegral w, Gray1.height = fromIntegral h,
		Gray1.body = bd } )
	where
	G {	width = w, height = h, xOffset = xo, yOffset = yo,
		delayNum = dn, delayDen = dd, disposeOp = dop, blendOp = bop,
		image = bd } = g

fromImages :: [(Gray1.G, Ratio Word16)] -> [G]
fromImages [] = error "no images"
fromImages ida@((i0, d0) : _) =
	firstImage i0 d0 : go 0 ida
	where
	go _ [] = error "no images"
	go _ [_] = []
	go d ((i1, _) : ids@((i2, d2) : _)) =
		case fromDiff i1 i2 (d + d2) of
			Nothing -> go (d + d2) ids
			Just g -> g : go 0 ids

firstImage :: Gray1.G -> Ratio Word16 -> G
firstImage Gray1.G { Gray1.width = w, Gray1.height = h, Gray1.body = bd } dly =
	G {
		width = fromIntegral w, height = fromIntegral h,
		xOffset = 0, yOffset = 0,
		delayNum = numerator dly, delayDen = denominator dly,
		disposeOp = 0, blendOp = 0, image = bd }

fromDiff :: Gray1.G -> Gray1.G -> Ratio Word16 -> Maybe G
fromDiff p c dly = do
	(xo, yo, Gray1.G {
		Gray1.width = w, Gray1.height = h, Gray1.body = bd }) <-
		Gray1.diff p c
	pure G {
		width = fromIntegral w, height = fromIntegral h,
		xOffset = xo, yOffset = yo,
		delayNum = numerator dly, delayDen = denominator dly,
		disposeOp = 0, blendOp = 0,
		image = bd }
