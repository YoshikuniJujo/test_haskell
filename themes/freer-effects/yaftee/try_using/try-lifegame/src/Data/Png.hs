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
	module Data.Png.Palette,
	module Data.Png.Filter,

	Datable(..)

	) where

import Data.ByteString.FingerTree qualified as BSF
import Data.Png.Header
import Data.Png.Palette
import Data.Png.Filter

class Datable a where
	isDat :: a -> Bool; endDat :: a -> Bool
	toDat :: Data.Png.Header.H -> a -> BSF.ByteString
