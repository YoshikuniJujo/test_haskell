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
	deriving (Show, Enum)

data DataClass
	= Primitive
	| Constructed
	deriving (Show, Enum)

------------------------------------------------------------

decode :: Analyzer BS.ByteString [Asn1]
decode = listAll $ decodeTag >>= \t@(Asn1Tag _ dc _) ->
	case dc of
		Primitive -> Asn1Prim t <$> decodePrim
		Constructed -> Asn1Cnst t <$> decodeCnst

------------------------------------------------------------

decodeTag :: Analyzer BS.ByteString Asn1Tag
decodeTag = decodeTag1 >>= \(tc, dc, mtn) -> case mtn of
	Just tn -> return .  Asn1Tag tc dc $ fromIntegral tn
	_ -> Asn1Tag tc dc <$> decodeTagR 0

decodeTag1 :: Analyzer
	BS.ByteString (TagClass, DataClass, Maybe Word8)
decodeTag1 = (<$> token) $ \w -> (
	toEnum . fromEnum $ w `shiftR` 6,
	toEnum . fromEnum $ testBit w 5,
	checkMaybe (/= 0x1f) $ w .&. 0x1f )

checkMaybe :: (a -> Bool) -> a -> Maybe a
checkMaybe p x = if p x then Just x else Nothing

decodeTagR :: Integer -> Analyzer BS.ByteString Integer
decodeTagR n = token >>= \w -> if testBit w 7
	then decodeTagR $
		n `shiftL` 7 .|. fromIntegral (w .&. 0x7f)
	else return $ n `shiftL` 7 .|. fromIntegral w

------------------------------------------------------------

decodePrim :: Analyzer BS.ByteString BS.ByteString
decodePrim = decodeLength >>= tokens

decodeCnst :: Analyzer BS.ByteString [Asn1]
decodeCnst = decodeLength >>= tokens >>= \bs ->
	case runAnalyzer decode bs of
		Right (as, "") -> return as
		Left err -> fail err
		_ -> error "Der.decodeCnst: never occur"

decodeLength :: Analyzer BS.ByteString Integer
decodeLength = token >>= \w -> if testBit w 7
	then decodeLengthN (w .&. 0x7f) 0
	else return $ fromIntegral w

decodeLengthN ::
	Word8 -> Integer -> Analyzer BS.ByteString Integer
decodeLengthN n ln
	| n >= 0x7f = fail $ "Der.decodeLengthN: " ++
		"the value 0b11111111 shall not be used"
	| n <= 0 = return ln
	| otherwise = token >>= decodeLengthN (n - 1) .
		(ln `shiftL` 8 .|.) . fromIntegral
