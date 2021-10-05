module Lib where

import Codec.Picture

readImageRGB8 :: FilePath -> IO (Image PixelRGB8)
readImageRGB8 fp = convertRGB8 . (\(Right r) -> r) <$> readImage fp

readImageRGBA8 :: FilePath -> IO (Image PixelRGBA8)
readImageRGBA8 fp = convertRGBA8 . (\(Right r) -> r) <$> readImage fp
