{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module FctlImageBody.Gray1Words (BodyGray1(..)) where

import Data.Png.Datable qualified as PngE
import Data.Word
import Data.ByteString.FingerTree qualified as BSF
import Data.Apng

data BodyGray1 = BodyGray1Fctl Fctl' | BodyGray1Pixels [Word8] deriving Show

instance Fctlable' BodyGray1 where
	getFctl' = \case BodyGray1Fctl f -> Just f; _ -> Nothing

instance PngE.Datable BodyGray1 where
	isDat = \case BodyGray1Pixels _ -> True; _ -> False
	endDat _ = False
	toDat _hdr = \case
		BodyGray1Pixels bs -> BSF.pack bs
		_ -> error "bad"
