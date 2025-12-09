{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Apng (

	-- * ACTL

	Actl(..), encodeActl,

	-- * FCTL

	Fctl(..), Fctlable(..), encodeFctl,
	fctlPoss,

	-- ** Dispose Op

	DisposeOp(..),

	pattern DisposeOpNone, pattern DisposeOpBackground,
	pattern DisposeOpPrevious,

	-- ** Blend Op

	BlendOp(..),

	pattern BlendOpSource, pattern BlendOpOver,

	-- * FCTL PIXELS GRAY 1

	FctlPixelsGray1(..)

	) where

import Data.Ratio
import Data.Word
import Data.ByteString.FingerTree qualified as BSF
import Data.ByteString.FingerTree.Bits qualified as BSF
import Data.Png.Header qualified as Header
import Data.Png.Datable qualified as Datable

-- ACTL

data Actl = Actl { actlFrames :: Word32, actlPlays :: Word32 } deriving Show

encodeActl :: Actl -> BSF.ByteString
encodeActl c = BSF.fromBitsBE' (actlFrames c) <> BSF.fromBitsBE' (actlPlays c)

-- FCTL

data Fctl = Fctl {
	fctlWidth :: Word32, fctlHeight :: Word32,
	fctlXOffset :: Word32, fctlYOffset :: Word32,
	fctlDelay :: Ratio Word16,
	fctlDisposeOp :: DisposeOp, fctlBlendOp :: BlendOp } deriving Show

class Fctlable a where getFctl :: a -> Maybe Fctl

encodeFctl :: Word32 -> Fctl -> BSF.ByteString
encodeFctl sn f =
	BSF.fromBitsBE' sn <>
	BSF.fromBitsBE' w <> BSF.fromBitsBE' h <>
	BSF.fromBitsBE' xo <> BSF.fromBitsBE' yo <>
	BSF.fromBitsBE' dn <> BSF.fromBitsBE' dd <>
	BSF.fromBitsBE' dop <> BSF.fromBitsBE' bop
	where
	w = fctlWidth f; h = fctlHeight f
	xo = fctlXOffset f; yo = fctlYOffset f
	dn = numerator $ fctlDelay f; dd = denominator $ fctlDelay f
	dop = unDisposeOp $ fctlDisposeOp f; bop = unBlendOp $ fctlBlendOp f

fctlPoss :: Header.H -> Fctl -> [[(Int, Int)]]
fctlPoss hdr fctl = Header.calcPoss' hdr (fctlWidth fctl) (fctlHeight fctl)

-- Dispose Op

newtype DisposeOp = DisposeOp { unDisposeOp :: Word8 } deriving Show

pattern DisposeOpNone, DisposeOpBackground, DisposeOpPrevious :: DisposeOp
pattern DisposeOpNone = DisposeOp 0
pattern DisposeOpBackground = DisposeOp 1
pattern DisposeOpPrevious = DisposeOp 2

-- Blend Op

newtype BlendOp = BlendOp { unBlendOp :: Word8 } deriving Show

pattern BlendOpSource, BlendOpOver :: BlendOp
pattern BlendOpSource = BlendOp 0
pattern BlendOpOver = BlendOp 1

-- FCTL PIXEL GRAY1

data FctlPixelsGray1
	= FctlPixelsGray1Fctl Fctl | FctlPixelsGray1Pixels [Word8] deriving Show

instance Fctlable FctlPixelsGray1 where
	getFctl = \case FctlPixelsGray1Fctl f -> Just f; _ -> Nothing

instance Datable.Datable FctlPixelsGray1 where
	isDat = \case FctlPixelsGray1Pixels _ -> True; _ -> False
	endDat _ = False
	toDat _hdr = \case
		FctlPixelsGray1Pixels bs -> BSF.pack bs
		_ -> error "bad"
