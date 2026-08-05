{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Hex where

import Data.Maybe
import Data.List qualified as L
import Data.Word
import Data.ByteString qualified as BS
import Numeric

newtype Hex = Hex BS.ByteString

instance Show Hex where show = toString

instance Read Hex where readsPrec _ = (: []) . (, "") . fromString

toString :: Hex -> String
toString (Hex bs) = ($ "") . foldr (.) id . map wordToHexString $ BS.unpack bs

wordToHexString :: Word8 -> ShowS
wordToHexString w = \s ->
	let	s' = showHex w ""
		l = length s' in
		replicate (2 - l) '0' ++ s' ++ s

fromString :: String -> Hex
fromString = Hex . BS.pack . L.unfoldr (listToMaybe . readHexWord)

readHexWord :: String -> [(Word8, String)]
readHexWord "" = []
readHexWord (c0 : c1 : cs) = do
	(w, "") <- readHex [c0, c1]
	pure (w, cs)
readHexWord _ = error "bad"
