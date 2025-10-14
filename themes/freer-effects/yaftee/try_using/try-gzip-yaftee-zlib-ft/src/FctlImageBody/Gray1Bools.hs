{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module FctlImageBody.Gray1Bools (

	BodyGray1(..)

	) where

import Data.Bits
import Data.Bool
import Data.Word
import Data.ByteString.FingerTree qualified as BSF

import Control.Monad.Yaftee.Pipe.Png.Encode qualified as PngE

import Control.Monad.Yaftee.Pipe.Apng.Decode

data BodyGray1 = BodyGray1Fctl Fctl | BodyGray1Pixels [Bool] deriving Show

instance Fctlable BodyGray1 where
	getFctl = \case BodyGray1Fctl f -> Just f; _ -> Nothing

instance PngE.Datable BodyGray1 where
	isDat = \case BodyGray1Pixels _ -> True; _ -> False
	endDat _ = False
	toDat _hdr = \case
		BodyGray1Pixels bs -> BSF.pack $ boolsToWords bs
		_ -> error "bad"

boolsToWords :: [Bool] -> [Word8]
boolsToWords = (boolsToWord <$>) . sep 8

boolsToWord :: [Bool] -> Word8
boolsToWord = go 0 . to8
	where
	go r [] = r
	go r (b : bs) = go (bool id (.|. 1) b (r `shiftL` 1)) bs
	to8 bs = bs ++ replicate (8 - Prelude.length bs) False

sep :: Int -> [a] -> [[a]]
sep _ [] = []
sep n xs = take n xs : sep n (drop n xs)
