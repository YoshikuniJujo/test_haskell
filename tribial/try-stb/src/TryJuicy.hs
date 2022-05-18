{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryJuicy where

import Codec.Picture

import qualified Data.ByteString as BS
import qualified Data.Vector.Storable as V

readRgba8 :: FilePath -> IO (Image PixelRGBA8)
readRgba8 fp = either error convertRGBA8 <$> readImage fp

getJuicyImageBytes :: Image PixelRGBA8 -> BS.ByteString
getJuicyImageBytes = BS.pack . V.toList . imageData

sampleJuicy :: IO BS.ByteString
sampleJuicy = getJuicyImageBytes <$> readRgba8 "../../files/images/texture.jpg"
