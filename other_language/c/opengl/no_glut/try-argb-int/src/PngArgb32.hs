{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module PngArgb32 where

import Data.CairoImage
import Data.JuicyCairo
import Codec.Picture

writePngArgb32 :: FilePath -> Argb32 -> IO ()
writePngArgb32 fp = writePng fp . cairoArgb32ToJuicyRGBA8
