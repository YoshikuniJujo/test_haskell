{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Codec.Bech32.ByteString (encode, decode) where

import Data.Word
import Data.ByteString qualified as BS

import Data.Word.Yj

import Codec.Bech32 qualified as Bech32

newtype Word5List = Word5List [Word5] deriving Show

encode :: String -> Bech32.B -> Either String BS.ByteString
encode hrp0 = (BS.pack <$>) . checkedToHdrDat hrp0

checkedToHdrDat :: String -> Bech32.B -> Either String [Word8]
checkedToHdrDat hrp0 (Bech32.B hrp dt)
	| hrp == hrp0 = word5sToWord8s dt
	| otherwise = Left $ "HRP should be " ++ show hrp0

data Separated = Separated {
	humanReadPart :: String,
	dataPart :: String }
	deriving Show

decode :: String -> BS.ByteString -> Bech32.B
decode hrp = Bech32.B hrp . word8sToWord5s . BS.unpack
