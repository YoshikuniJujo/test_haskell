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

	Fctl'(..), Fctlable'(..), encodeFctl',
	fctlPoss',

	-- ** Dispose Op

	DisposeOp(..),

	pattern DisposeOpNone, pattern DisposeOpBackground,
	pattern DisposeOpPrevious,

	-- ** Blend Op

	BlendOp(..),

	pattern BlendOpSource, pattern BlendOpOver,

	) where

import Data.Ratio
import Data.Word
import Data.ByteString.FingerTree qualified as BSF
import Data.ByteString.FingerTree.Bits qualified as BSF
import Data.Png.Header qualified as Header
import Data.Png.Header.Data qualified as Header

data Fctl = Fctl {
	fctlWidth :: Word32, fctlHeight :: Word32,
	fctlXOffset :: Word32, fctlYOffset :: Word32,
	fctlDelayNum :: Word16, fctlDelayDen :: Word16,
	fctlDisposeOp :: Word8, fctlBlendOp :: Word8 } deriving Show

fctl'ToFctl :: Fctl' -> Fctl
fctl'ToFctl f = Fctl {
	fctlWidth = fctlWidth' f, fctlHeight = fctlHeight' f,
	fctlXOffset = fctlXOffset' f, fctlYOffset = fctlYOffset' f,
	fctlDelayNum = numerator $ fctlDelay f,
	fctlDelayDen = denominator $ fctlDelay f,
	fctlDisposeOp = unDisposeOp $ fctlDisposeOp' f,
	fctlBlendOp = unBlendOp $ fctlBlendOp' f }

data Fctl' = Fctl' {
	fctlWidth' :: Word32, fctlHeight' :: Word32,
	fctlXOffset' :: Word32, fctlYOffset' :: Word32,
	fctlDelay :: Ratio Word16,
	fctlDisposeOp' :: DisposeOp, fctlBlendOp' :: BlendOp } deriving Show

newtype DisposeOp = DisposeOp { unDisposeOp :: Word8 } deriving Show

newtype BlendOp = BlendOp { unBlendOp :: Word8 } deriving Show

encodeFctl :: Word32 -> Fctl -> BSF.ByteString
encodeFctl sn c =
	BSF.fromBitsBE' sn <>
	BSF.fromBitsBE' (fctlWidth c) <> BSF.fromBitsBE' (fctlHeight c) <>
	BSF.fromBitsBE' (fctlXOffset c) <> BSF.fromBitsBE' (fctlYOffset c) <>
	BSF.fromBitsBE' (fctlDelayNum c) <> BSF.fromBitsBE' (fctlDelayDen c) <>
	BSF.fromBitsBE' (fctlDisposeOp c) <> BSF.fromBitsBE' (fctlBlendOp c)

encodeFctl' :: Word32 -> Fctl' -> BSF.ByteString
encodeFctl' sn = encodeFctl sn . fctl'ToFctl

data Actl = Actl {
	actlFrames :: Word32,
	actlPlays :: Word32 } deriving Show

encodeActl :: Actl -> BSF.ByteString
encodeActl c = BSF.fromBitsBE' (actlFrames c) <> BSF.fromBitsBE' (actlPlays c)

pattern BlendOpSource, BlendOpOver :: BlendOp
pattern BlendOpSource = BlendOp 0
pattern BlendOpOver = BlendOp 1

pattern DisposeOpNone, DisposeOpBackground, DisposeOpPrevious :: DisposeOp
pattern DisposeOpNone = DisposeOp 0
pattern DisposeOpBackground = DisposeOp 1
pattern DisposeOpPrevious = DisposeOp 2

fctlPoss' :: Header.H -> Fctl' -> [[(Int, Int)]]
fctlPoss' hdr fctl = Header.calcPoss' hdr (fctlWidth fctl') (fctlHeight fctl')
	where fctl' = fctl'ToFctl fctl

class Fctlable' a where getFctl' :: a -> Maybe Fctl'
