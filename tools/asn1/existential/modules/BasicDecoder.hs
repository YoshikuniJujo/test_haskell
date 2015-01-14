{-# LANGUAGE
	OverloadedStrings,
	ExistentialQuantification #-}

module BasicDecoder (
	Asn1Tag(..), TagClass(..), DataClass(..),
	decodeTag, decodeLength ) where

import Control.Applicative
import Control.Monad
import Data.Bits
import Data.Word8

import qualified Data.ByteString as BS

import Analyzer
import Asn1Tag

------------------------------------------------------------

decodeTag :: Analyzer BS.ByteString Asn1Tag
decodeTag = decodeTag1 >>= \(tc, dc, mtn) -> maybe
	(Asn1Tag tc dc <$> decodeTagR0)
	(return . Asn1Tag tc dc . fromIntegral)
	mtn

decodeTag1 :: Analyzer BS.ByteString
	(TagClass, DataClass, Maybe Word8)
decodeTag1 = flip fmap token $ \w -> let
	tc = toEnum . fromIntegral $ w `shiftR` 6
	dc = if testBit w 5 then Constructed else Primitive
	tn = case w .&. 0x1f of
		0x1f -> Nothing
		n	| n < 0x1f -> Just n
			| otherwise ->
				error "never occur" in
	(tc, dc, tn)

decodeTagR0 :: Analyzer BS.ByteString Integer
decodeTagR0 = decodeTagR 0 >>= \n -> do
	when (n <= 30) $ fail
		"Use single byte for tag number 0 - 30"
	return n

decodeTagR :: Integer -> Analyzer BS.ByteString Integer
decodeTagR 0 = token >>= \w -> do
	when (w == 0x80) $ fail
		"Redundant byte for tag number"
	if testBit w 7
		then decodeTagR $ fromIntegral (w .&. 0x7f)
		else return $ fromIntegral w
decodeTagR n = token >>= \w -> if testBit w 7
	then decodeTagR $
		n `shiftL` 7 .|. fromIntegral (w .&. 0x7f)
	else return $ n `shiftL` 7 .|. fromIntegral w

decodeLength :: Analyzer BS.ByteString (Maybe Integer)
decodeLength = decodeLength1 >>= \ln1 -> case ln1 of
	Just (Right ln) -> return . Just $ fromIntegral ln
	Just (Left n) -> Just <$> decodeLengthR 0 n
	_ -> return Nothing

decodeLength1 :: Analyzer BS.ByteString
	(Maybe (Either Word8 Word8))
decodeLength1 = flip fmap token $ \w -> if not (testBit w 7)
	then Just $ Right w
	else let ln = w .&. 0x7f in
		if ln /= 0 then Just $ Left ln else Nothing

decodeLengthR ::
	Integer -> Word8 -> Analyzer BS.ByteString Integer
decodeLengthR ln 0 = return ln
decodeLengthR ln n = token >>= \w -> decodeLengthR
	(ln `shiftL` 8 .|. fromIntegral w)
	(n - 1)
