{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trials.Followbox.Image where

import qualified Data.ByteString.Lazy as LBS
import qualified Codec.Picture as JP
import qualified Codec.Picture.Extra as JP

bsToImage :: LBS.ByteString -> Either String (JP.Image JP.PixelRGBA8)
bsToImage lbs = JP.convertRGBA8 <$> JP.decodeImage (LBS.toStrict lbs)

scale :: Integer -> Integer -> JP.Image JP.PixelRGBA8 -> JP.Image JP.PixelRGBA8
scale w_ h_ = JP.scaleBilinear w h where [w, h] = fromIntegral <$> [w_, h_]
