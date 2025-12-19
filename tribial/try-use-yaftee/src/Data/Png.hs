{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Png (

	module Data.Png.Header,

	Datable(..), PixelsGray1(..)

	) where

import Data.Word
import Data.ByteString.FingerTree qualified as BSF
import Data.Png.Header

class Datable a where
	isDat :: a -> Bool; endDat :: a -> Bool
	toDat :: Data.Png.Header.Header -> a -> BSF.ByteString

-- FCTL PIXEL GRAY1

data PixelsGray1 = PG1Pixels [Word8] deriving Show

instance Datable PixelsGray1 where
	isDat (PG1Pixels _) = True; endDat (PG1Pixels _) = False
	toDat _ (PG1Pixels bs) = BSF.pack bs
