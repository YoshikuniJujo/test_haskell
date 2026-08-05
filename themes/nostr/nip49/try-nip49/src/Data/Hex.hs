{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Hex where

import Data.Word
import Data.ByteString qualified as BS
import Numeric

newtype Hex = Hex BS.ByteString

instance Show Hex where show = toString

toString :: Hex -> String
toString (Hex bs) = ($ "") . foldr (.) id . map showHex $ BS.unpack bs

wordToHexString :: Word8 -> ShowS
wordToHexString = showHex
