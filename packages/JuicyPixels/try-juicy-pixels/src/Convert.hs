{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Convert where

import Codec.Picture

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

convert :: IO ()
convert = do
	eimg <- readImage "../../../files/images/flower.jpg"
	case eimg of
		Left err -> error err
		Right img -> writePng "flower.png" $ convertRGB16 img

convertByteString :: BS.ByteString -> Either String BS.ByteString
convertByteString jpg = LBS.toStrict <$> (encodeDynamicPng =<< decodeImage jpg)

convert2 :: BS.ByteString -> Either String BS.ByteString
convert2 img = LBS.toStrict . encodePng . convertRGB8 <$> decodeImage img
