{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Image.Immutable where

import Data.Vector qualified as V
import Data.Word

data Gray = Gray {
	grayWidth :: Int, grayHeight :: Int, grayBody :: V.Vector Word8 }

grayRead :: Gray -> Int -> Int -> Word8
grayRead Gray { grayWidth = w, grayHeight = _h, grayBody = bd } x y =
	bd V.! (y * w + x)
