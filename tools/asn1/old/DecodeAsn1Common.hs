{-# LANGUAGE OverloadedStrings #-}

module DecodeAsn1Common
	(Asn1(..), decode1, decodeTag, decodeLength, next, listAll) where

import Analyzer
import Data.Bits
import Data.Word8

import qualified Data.ByteString as BS

data Asn1 = Asn1 Asn1Tag BS.ByteString deriving Show

data Asn1Tag = Asn1Tag TagClass DataClass Integer deriving Show

data TagClass = Universal | Application | ContextSpecific | Private deriving Show

data DataClass = Primitive | Constructed deriving Show

decode1 :: Analyzer Asn1
decode1 = uncurry Asn1 `build` (decodeTag `next` decodeContents)

decodeTag :: Analyzer Asn1Tag
decodeTag = decodeTag1 `bind` \(tc, dc, mtn) ->
	case mtn of
		Just tn -> ret . Asn1Tag tc dc $ fromIntegral tn
		_ -> Asn1Tag tc dc `build` decodeTagR 0

decodeTag1 :: Analyzer (TagClass, DataClass, Maybe Word8)
decodeTag1 = anl `build` spot (const True)
	where
	anl w = (tc, dc, ln)
		where
		tc = case w `shiftR` 6 of
			0 -> Universal; 1 -> Application
			2 -> ContextSpecific; 3 -> Private
			_ -> error "never occur"
		dc = case (w .&. 0x3f) `shiftR` 5 of
			0 -> Primitive; 1 -> Constructed
			_ -> error "never occur"
		ln = case w .&. 0x1f of
			0x1f -> Nothing
			n	| n < 0x1f -> Just n
				| otherwise -> error "never occur"

decodeTagR :: Integer -> Analyzer Integer
decodeTagR n = spot (const True) `bind` \b ->
	if b `shiftR` 7 == 0
		then ret $ n `shiftL` 7 .|. fromIntegral b
		else decodeTagR (n `shiftL` 7 .|. fromIntegral (b .&. 0x7f))

decodeContents :: Analyzer BS.ByteString
decodeContents = decodeLength `bind` takeByteString

decodeLength :: Analyzer Integer
decodeLength = spot (const True) `bind` \b ->
	if b `shiftR` 7 == 0
		then ret $ fromIntegral b
		else decodeLengthN (b .&. 0x7f) 0

decodeLengthN :: Word8 -> Integer -> Analyzer Integer
decodeLengthN n ln
	| n >= 0x7f = failure
	| n <= 0 = ret ln
	| otherwise = spot (const True) `bind` \b ->
		decodeLengthN (n - 1) (ln `shiftL` 8 .|. fromIntegral b)

takeByteString :: Integer -> Analyzer BS.ByteString
takeByteString n
	| n <= 0 = ret ""
	| otherwise = spot (const True) `bind` \b ->
		(b `BS.cons`) `build` takeByteString (n - 1)
