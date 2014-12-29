module DecodeAsn1Common (decodeTag1) where

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
decodeTag = undefined

decodeTag1 :: Analyzer (TagClass, DataClass, Maybe Word8)
decodeTag1 = anl `build` spot (const True)
	where
	anl w = (tc, dc, ln)
		where
		tc = case w `shiftR` 6 of
			0 -> Universal; 1 -> Application
			2 -> ContextSpecific; 3 -> Private
			_ -> error "never occur"
		dc = case w .&. 0x3f `shiftR` 5 of
			0 -> Primitive; 1 -> Constructed
			_ -> error "never occur"
		ln = case w .&. 0x1f of
			0x1f -> Nothing
			n	| n < 0x1f -> Just n
				| otherwise -> error "never occur"

decodeContents :: Analyzer BS.ByteString
decodeContents = undefined
