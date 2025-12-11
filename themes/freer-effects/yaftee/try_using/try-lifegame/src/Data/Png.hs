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
	module Data.Png.Filter,

	Datable(..),
	Palette(..), encodePalette

	) where

import Data.Vector qualified as V
import Data.Word
import Data.ByteString.FingerTree qualified as BSF
import Data.Png.Header
import Data.Png.Filter

class Datable a where
	isDat :: a -> Bool; endDat :: a -> Bool
	toDat :: Data.Png.Header.Header -> a -> BSF.ByteString

data Palette = Palette (V.Vector (Word8, Word8, Word8)) deriving Show

encodePalette :: Palette -> BSF.ByteString
encodePalette (Palette v) =
	foldl' (\bs (r, g, b) -> bs <> BSF.pack [r, g, b]) BSF.empty v
