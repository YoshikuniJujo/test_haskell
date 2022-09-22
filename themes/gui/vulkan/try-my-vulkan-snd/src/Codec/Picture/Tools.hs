{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Codec.Picture.Tools (
	Image(..), PixelRGBA8, readRgba8
	) where

import Codec.Picture

readRgba8 :: FilePath -> IO (Image PixelRGBA8)
readRgba8 fp = either error convertRGBA8 <$> readImage fp
