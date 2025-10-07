{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module FctlImage where

import Control.Monad.Yaftee.Pipe.Apng.Decode qualified as Decode
import Data.Word
import Data.Image.Simple qualified as Image
import Data.Image.Immutable qualified as ImageI
import Data.Vector qualified as V

data GrayM s = GrayM {
	grayMWidth :: Word32, grayMHeight :: Word32,
	grayMXOffset :: Word32, grayMYOffset :: Word32,
	grayMDelayNum :: Word16, grayMDelayDen :: Word16,
	grayMDisposeOP :: Word8, grayMBlendOp :: Word8,
	grayMImage :: V.MVector s Word8 }

data GrayI = GrayI {
	grayIWidth :: Word32, grayIHeight :: Word32,
	grayIXOffset :: Word32, grayIYOffset :: Word32,
	grayIDelayNum :: Word16, grayIDelayDen :: Word16,
	grayIDisposeOP :: Word8, grayIBlendOp :: Word8,
	grayIImage :: V.Vector Word8 }
