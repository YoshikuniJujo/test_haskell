{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Apng (

	Actl(..), encodeActl,
	Fctl(..), encodeFctl, encodeFctl',

	disposeOpNone, disposeOpBackground, disposeOpPrevious,
	blendOpSource, blendOpOver

	) where

import Data.Word
import Data.ByteString.FingerTree qualified as BSF
import Data.ByteString.FingerTree.Bits qualified as BSF

data Fctl = Fctl {
	fctlSequenceNumber :: Word32,
	fctlWidth :: Word32, fctlHeight :: Word32,
	fctlXOffset :: Word32, fctlYOffset :: Word32,
	fctlDelayNum :: Word16, fctlDelayDen :: Word16,
	fctlDisposeOp :: Word8, fctlBlendOp :: Word8 } deriving Show

encodeFctl :: Fctl -> BSF.ByteString
encodeFctl c =
	BSF.fromBitsBE' (fctlSequenceNumber c) <>
	BSF.fromBitsBE' (fctlWidth c) <> BSF.fromBitsBE' (fctlHeight c) <>
	BSF.fromBitsBE' (fctlXOffset c) <> BSF.fromBitsBE' (fctlYOffset c) <>
	BSF.fromBitsBE' (fctlDelayNum c) <> BSF.fromBitsBE' (fctlDelayDen c) <>
	BSF.fromBitsBE' (fctlDisposeOp c) <> BSF.fromBitsBE' (fctlBlendOp c)

encodeFctl' :: Word32 -> Fctl -> BSF.ByteString
encodeFctl' sn c =
	BSF.fromBitsBE' sn <>
	BSF.fromBitsBE' (fctlWidth c) <> BSF.fromBitsBE' (fctlHeight c) <>
	BSF.fromBitsBE' (fctlXOffset c) <> BSF.fromBitsBE' (fctlYOffset c) <>
	BSF.fromBitsBE' (fctlDelayNum c) <> BSF.fromBitsBE' (fctlDelayDen c) <>
	BSF.fromBitsBE' (fctlDisposeOp c) <> BSF.fromBitsBE' (fctlBlendOp c)

data Actl = Actl {
	actlFrames :: Word32,
	actlPlays :: Word32 } deriving Show

encodeActl :: Actl -> BSF.ByteString
encodeActl c = BSF.fromBitsBE' (actlFrames c) <> BSF.fromBitsBE' (actlPlays c)

disposeOpNone, disposeOpBackground, disposeOpPrevious :: Word8
disposeOpNone = 0; disposeOpBackground = 1; disposeOpPrevious = 2

blendOpSource, blendOpOver :: Word8
blendOpSource = 0; blendOpOver = 1
