{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Scrypt where

import Data.Maybe
import Data.Word
import Data.ByteString qualified as BS
import System.Entropy
import Crypto.Scrypt qualified as Scr

params :: Word8 -> Maybe Scr.ScryptParams
params logN = Scr.scryptParamsLen (fromIntegral logN) 8 1 32

scryptIO :: Word8 -> BS.ByteString -> IO Scrypted
scryptIO logN pss = do
	slt <- getEntropy 16
	pure Scrypted {
		salt = slt,
		pass = Scr.getHash $ Scr.scrypt
			(fromJust $ params logN) (Scr.Salt slt) (Scr.Pass pss) }

data Scrypted = Scrypted { salt :: BS.ByteString, pass :: BS.ByteString }
	deriving Show

hash :: Word8 -> BS.ByteString -> BS.ByteString -> BS.ByteString
hash lgn slt pss = Scr.getHash $ Scr.scrypt
	(fromJust $ params lgn) (Scr.Salt slt) (Scr.Pass pss)
