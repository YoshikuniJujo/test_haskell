{-# LANGUAGE OverloadedStrings, TypeFamilies #-}

import Control.Applicative ((<$>), (<*>))
import Data.Bits (testBit, shiftL, shiftR, (.&.), (.|.))
import Data.Word8 (Word8)
import System.IO.Unsafe (unsafePerformIO)

import qualified ListLike as LL
import qualified Data.ByteString as BS

import Analyzer

data Asn1 = Asn1 Asn1Tag BS.ByteString
	deriving Show

data Asn1Tag
	= Asn1Tag TagClass DataClass Integer
	deriving Show

data TagClass
	= Universal
	| Application
	| ContextSpecific
	| Private
	deriving Show

data DataClass
	= Primitive
	| Constructed
	deriving Show

cert :: BS.ByteString
cert = unsafePerformIO $ BS.readFile "test_ASN_1_cert.der"

cert1 :: BS.ByteString
Right ((_, cert1), _) = runAnalyzer
	((,) <$> decodeTag <*> decodeContents) cert

decode :: BS.ByteString -> Maybe [Asn1]
decode bs = case runAnalyzer (listAll $
		Asn1 <$> decodeTag <*> decodeContents) bs of
	Right (a, "") -> Just a
	_ -> Nothing

decodeAsn1 :: Asn1 -> Maybe [Asn1]
decodeAsn1 (Asn1 _ bs) = decode bs

decodeTag :: (LL.ListLike a, LL.Element a ~ Word8) =>
	Analyzer a Asn1Tag
decodeTag = do
	(tc, dc, mtn) <- decodeTag1
	case mtn of
		Just tn -> return .
			Asn1Tag tc dc $ fromIntegral tn
		_ -> Asn1Tag tc dc <$> decodeTagR 0

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
				| otherwise ->
					error "never occur"
	return (tc, dc, tn)

decodeTagR :: (LL.ListLike a, LL.Element a ~ Word8) =>
	Integer -> Analyzer a Integer
decodeTagR n = do
	w <- token
	if testBit w 7
		then decodeTagR $ n `shiftL` 7 .|.
			fromIntegral (w .&. 0x7f)
		else return $ n `shiftL` 7 .|.
			fromIntegral w

decodeContents :: (LL.ListLike a, LL.Element a ~ Word8) =>
	Analyzer a a
decodeContents = decodeLength >>= tokens

decodeLength :: (LL.ListLike a, LL.Element a ~ Word8) =>
	Analyzer a Integer
decodeLength = do
	w <- token
	if testBit w 7
		then decodeLengthN (w .&. 0x7f) 0
		else return $ fromIntegral w

decodeLengthN :: (LL.ListLike a, LL.Element a ~ Word8) =>
	Word8 -> Integer -> Analyzer a Integer
decodeLengthN n ln
	| n >= 0x7f = fail "bad"
	| n <= 0 = return ln
	| otherwise = do
		w <- token
		decodeLengthN (n - 1) $ ln `shiftL` 8 .|.
			fromIntegral w
