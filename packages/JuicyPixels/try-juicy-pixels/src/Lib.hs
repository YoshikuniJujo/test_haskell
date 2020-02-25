module Lib where

import Codec.Picture
import Network.HTTP.Simple

downloadImage :: Request -> IO (Either String (Image PixelRGBA8))
downloadImage = ((convertRGBA8 <$>) . decodeImage . getResponseBody <$>) . httpBS
