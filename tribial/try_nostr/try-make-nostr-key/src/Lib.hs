{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Data.Word.Wider
import Data.ByteString qualified as BS
import Data.Text qualified as T
import System.Entropy qualified as E
import Codec.Binary.Bech32
import Crypto.Curve.Secp256k1

someFunc :: IO ()
someFunc = putStrLn "someFunc"

pair :: IO (BS.ByteString, Wider, Pub)
pair = do
	skbs <- E.getEntropy 32
	Just sk <- pure $ parse_int256 skbs
	Just pb <- pure $ derive_pub sk
	pure (skbs, sk, pb)

encodePubFromBS :: BS.ByteString -> Either EncodingError T.Text
encodePubFromBS pb = encode
	(either (error . show) id $ humanReadablePartFromText "npub")
	(dataPartFromBytes $ BS.tail pb)

encodeSecFromBS :: BS.ByteString -> Either EncodingError T.Text
encodeSecFromBS sc = encode
	(either (error . show) id $ humanReadablePartFromText "nsec")
	(dataPartFromBytes sc)

encodePub :: Pub -> Either EncodingError T.Text
encodePub = encodePubFromBS . serialize_point

pair' = do
	(skbs, _, pb) <- pair
	pure $ (,) <$> encodeSecFromBS skbs <*> encodePub pb
