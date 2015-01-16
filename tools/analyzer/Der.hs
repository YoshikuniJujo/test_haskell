{-# LANGUAGE OverloadedStrings #-}

module Der (
	Asn1(..), Asn1Tag(..), TagClass(..), DataClass(..),
	decode ) where

import Control.Applicative ((<$>))
import Data.Bits (testBit, shiftL, shiftR, (.&.), (.|.))
import Data.Word8 (Word8)

import qualified Data.ByteString as BS

import Analyzer (Analyzer(..), token, tokens, listAll)

------------------------------------------------------------

data Asn1
	= Asn1Cnst Asn1Tag [Asn1]
	| Asn1Prim Asn1Tag BS.ByteString
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

------------------------------------------------------------

decode :: Analyzer BS.ByteString [Asn1]
decode = listAll $ decodeTag >>= \t@(Asn1Tag _ dc _) ->
	case dc of
		Primitive ->
			Asn1Prim t <$> decodePrimitive
		Constructed ->
			Asn1Cnst t <$> decodeConstructed

------------------------------------------------------------

decodeTag :: Analyzer BS.ByteString Asn1Tag
decodeTag = do
	(tc, dc, mtn) <- decodeTag1
	case mtn of
		Just tn -> return .
			Asn1Tag tc dc $ fromIntegral tn
		_ -> Asn1Tag tc dc <$> decodeTagR 0

decodeTag1 :: Analyzer
	BS.ByteString (TagClass, DataClass, Maybe Word8)
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

decodeTagR :: Integer -> Analyzer BS.ByteString Integer
decodeTagR n = do
	w <- token
	if testBit w 7
		then decodeTagR $ n `shiftL` 7 .|.
			fromIntegral (w .&. 0x7f)
		else return $ n `shiftL` 7 .|.
			fromIntegral w

------------------------------------------------------------

decodePrimitive :: Analyzer BS.ByteString BS.ByteString
decodePrimitive = decodeLength >>= tokens

decodeConstructed :: Analyzer BS.ByteString [Asn1]
decodeConstructed = do
	bs <- decodeLength >>= tokens
	case runAnalyzer decode bs of
		Right (as, "") -> return as
		Left err -> fail err
		_ -> error "never occur"

decodeLength :: Analyzer BS.ByteString Integer
decodeLength = do
	w <- token
	if testBit w 7
		then decodeLengthN (w .&. 0x7f) 0
		else return $ fromIntegral w

decodeLengthN ::
	Word8 -> Integer -> Analyzer BS.ByteString Integer
decodeLengthN n ln
	| n >= 0x7f = fail "bad"
	| n <= 0 = return ln
	| otherwise = do
		w <- token
		decodeLengthN (n - 1) $ ln `shiftL` 8 .|.
			fromIntegral w
