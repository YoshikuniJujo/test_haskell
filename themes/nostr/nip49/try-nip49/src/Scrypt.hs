{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Scrypt (hash) where

import Data.Maybe
import Data.Word
import Data.ByteString qualified as BS
import Crypto.Scrypt qualified as Scr

hash :: Word8 -> BS.ByteString -> BS.ByteString -> BS.ByteString
hash (fromJust . params -> prms) (Scr.Salt -> slt) (Scr.Pass -> pss) =
	Scr.getHash $ Scr.scrypt prms slt pss

params :: Word8 -> Maybe Scr.ScryptParams
params lgn = Scr.scryptParamsLen (fromIntegral lgn) 8 1 32
