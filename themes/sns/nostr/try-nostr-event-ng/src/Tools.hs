{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Tools where

import Data.Bool
import Data.Char
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.Text qualified as T
import Numeric
import Crypto.Curve.Secp256k1

hexToPubkey :: T.Text -> Maybe Pub
hexToPubkey = parse_point
	. BS.pack . (fst . head . readHex <$>) . separate 2 . T.unpack

bsToHexText :: BSC.ByteString -> T.Text
bsToHexText = T.pack . strToHexStr . BSC.unpack

{-
hexTextToBs :: T.Text -> BSC.ByteString
hexTextToBs = BSC.pack . hexStrToStr . T.unpack
-}

fromHex :: T.Text -> BS.ByteString
fromHex = BS.pack . (fst . head . readHex <$>) . separate 2 . T.unpack

strToHexStr :: String -> String
strToHexStr = concat . (sh <$>) . map ord
	where
	sh n = let s = showHex n "" in replicate (2 - length s) '0' ++ s

separate :: Int -> String -> [String]
separate _ "" = []
separate n s = take n s : separate n (drop n s)

chomp :: T.Text -> T.Text
chomp = bool id T.init <$> (== '\n') . T.last <*> id
