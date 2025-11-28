{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.Png.Palette (Palette(..), encodePalette) where

import Data.Vector qualified as V
import Data.Word
import Data.ByteString.FingerTree qualified as BSF

data Palette = Palette (V.Vector (Word8, Word8, Word8)) deriving Show

encodePalette :: Palette -> BSF.ByteString
encodePalette (Palette v) =
	foldl' (\bs (r, g, b) -> bs <> BSF.pack [r, g, b]) BSF.empty v
