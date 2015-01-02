{-# LANGUAGE TypeFamilies #-}

module DecodeAsn1Common (runAnalyzer, decode1, decodeTag1, decodeTag) where

import Control.Applicative
import Data.Bits
import Data.Word8

import qualified Data.ByteString as BS

import Analyzer
import qualified ListLike as LL

data Asn1 = Asn1 Asn1Tag BS.ByteString deriving Show
data Asn1Tag = Asn1Tag TagClass DataClass Integer deriving Show
data TagClass = Universal | Application | ContextSpecific | Private deriving Show
data DataClass = Primitive | Constructed deriving Show

decode1 :: (LL.ListLike a, LL.Element a ~ Word8) => Analyzer a Asn1
decode1 = Asn1 <$> decodeTag <*> decodeContents

decodeTag :: (LL.ListLike a, LL.Element a ~ Word8) => Analyzer a Asn1Tag
decodeTag = do
	(tc, dc, mtn) <- decodeTag1
	maybe (Asn1Tag tc dc <$> decodeTagR 0)
		(return . Asn1Tag tc dc . fromIntegral) mtn

decodeTag1 :: (LL.ListLike a, LL.Element a ~ Word8) =>
	Analyzer a (TagClass, DataClass, Maybe Word8)
decodeTag1 = do
	w <- token
	let	tc = case w `shiftR` 6 of
			0 -> Universal
			1 -> Application
			2 -> ContextSpecific
			3 -> Private
			_ -> error "never occur"
		dc = case (w .&. 0x3f) `shiftR` 5 of
			0 -> Primitive
			1 -> Constructed
			_ -> error "never occur"
		tn = case w .&. 0x1f of
			0x1f -> Nothing
			n	| n < 0x1f -> Just n
				| otherwise -> error "never_occur"
	return (tc, dc, tn)

decodeTagR :: (LL.ListLike a, LL.Element a ~ Word8) => Integer -> Analyzer a Integer
decodeTagR n = do
	w <- token
	if testBit w 7
		then decodeTagR $ n `shiftL` 7 .|. fromIntegral (w .&. 0x7f)
		else return $ n `shiftL` 7 .|. fromIntegral w

decodeContents :: (LL.ListLike a, LL.Element a ~ Word8) =>
	Analyzer a BS.ByteString
decodeContents = undefined
