{-# LANGUAGE
	OverloadedStrings,
	ExistentialQuantification #-}

module BasicEncoder (
	Asn1Tag(..), TagClass(..), DataClass(..),
	encodeTag, encodeLength, integerToWord8s ) where

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

encodeLength :: Int -> Maybe Integer -> BS.ByteString
encodeLength _ (Just l)
	| l < 0 = error "No negative length"
encodeLength 0 (Just l)
	| l < 128 = BS.pack [fromIntegral l]
	| otherwise = BS.pack $
		(0x80 .|. fromIntegral (length ws)) : ws
	where
	ws = reverse $ integerToWord8s l
encodeLength n (Just l)
	| ln >= n = BS.pack $
		(0x80 .|. fromIntegral ln) : ws
	| otherwise = BS.pack $ (0x80 .|. fromIntegral n) :
		replicate (n - ln) 0x00 ++ ws
	where
	ws = reverse $ integerToWord8s l
	ln = length ws
encodeLength _ _ = "\x80"

integerToWord8s :: Integer -> [Word8]
integerToWord8s 0 = []
integerToWord8s n = fromIntegral (n .&. 0xff) :
	integerToWord8s (n `shiftR` 8)
