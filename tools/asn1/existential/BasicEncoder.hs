{-# LANGUAGE
	OverloadedStrings,
	ExistentialQuantification #-}

module BasicDecoder (
	Asn1Tag(..), TagClass(..), DataClass(..),
	encodeTag, encodeLength ) where

import Data.Bits
import Data.Word8

import qualified Data.ByteString as BS

import Asn1Tag

------------------------------------------------------------

encodeTag :: Asn1Tag -> BS.ByteString
encodeTag (Asn1Tag tc dc n)
	| n < 31 = BS.pack [c .|. fromIntegral n]
	| otherwise = BS.pack $ (c .|. 0x1f) : encodeTagR n
	where
	c = fromIntegral $ fromEnum tc `shiftL` 6 .|.
		fromEnum dc `shiftL` 5

encodeTagR :: Integer -> [Word8]
encodeTagR n = map (0x80 .|.) (reverse ws) ++ [w]
	where
	w : ws = integerToWord7s n

integerToWord7s :: Integer -> [Word8]
integerToWord7s 0 = []
integerToWord7s n = fromIntegral (n .&. 0x7f) :
	integerToWord7s (n `shiftR` 7)

encodeLength :: Maybe Integer -> BS.ByteString
encodeLength (Just n)
	| n < 0 = error "No negative length"
	| n < 128 = BS.pack [fromIntegral n]
	| otherwise = BS.pack $
		(0x80 .|. fromIntegral (length ws)) : ws
	where
	ws = reverse $ integerToWord8s n
encodeLength _ = "\x80"

integerToWord8s :: Integer -> [Word8]
integerToWord8s 0 = []
integerToWord8s n = fromIntegral (n .&. 0xff) :
	integerToWord8s (n `shiftR` 8)
