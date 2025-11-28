{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Fctl (

	fctlPoss', Fctlable(..)

	) where

import Data.Word
import Data.ByteString.FingerTree qualified as BSF
import Data.Color
import Data.Png.Header qualified as Header
import Data.Png.Header.Data qualified as Header
import Data.Apng

import Control.Monad.Yaftee.Pipe.Png.Encode qualified as PngE

newtype FrameNumber = FrameNumber Int deriving Show

data Body
	= BodyNull | BodyEnd | BodyFdatEnd
	| BodyFctl Fctl | BodyRgba [Rgba Double] deriving Show

data BodyGray
	= BodyGrayNull | BodyGrayEnd | BodyGrayFdatEnd
	| BodyGrayFctl Fctl | BodyGrayPixels [Word8] deriving Show

class Fctlable a where getFctl :: a -> Maybe Fctl

instance PngE.Datable Body where
	isDat = \case BodyRgba _ -> True; _ -> False
	endDat = \case BodyFdatEnd -> True; _ -> False
	toDat hdr = \case
		BodyRgba rgba -> BSF.pack $ Header.rgbaListToWord8List hdr rgba
		_ -> error "bad"

instance PngE.Datable BodyGray where
	isDat = \case BodyGrayPixels _ -> True; _ -> False
	endDat = \case BodyGrayFdatEnd -> True; _ -> False
	toDat _hdr = \case
		BodyGrayPixels g -> BSF.pack g
		_ -> error "bad"

fctlPoss' :: Header.Header -> Fctl -> [[(Int, Int)]]
fctlPoss' hdr fctl = Header.calcPoss' hdr (fctlWidth fctl) (fctlHeight fctl)
