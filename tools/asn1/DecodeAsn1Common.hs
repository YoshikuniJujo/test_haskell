{-# LANGUAGE OverloadedStrings, TypeFamilies #-}

module DecodeAsn1Common (
	runAnalyzer,
	Asn1(..), Asn1Data(..), RecFlag(..),
	decode1, decodeTag, decodeTag1,
	decodeLength, decodeLength1, decodeLengthR,
	) where

import Control.Applicative
import Control.Monad
import Data.Bits
import Data.Word8

import qualified Data.ByteString as BS

import Analyzer

data Asn1
	= Asn1 Asn1Tag Asn1Data
	deriving (Show, Eq)

data Asn1Tag
	= Asn1Tag TagClass DataClass Integer
	deriving (Show, Eq)

data TagClass
	= Universal
	| Application
	| ContextSpecific
	| Private
	deriving (Show, Eq)

data DataClass
	= Primitive
	| Constructed
	deriving (Show, Eq)

data Asn1Data
	= Asn1DataAsn1 [Asn1]
	| Asn1DataRaw BS.ByteString
	deriving (Show, Eq)

data RecFlag
	= NoRec
	| Rec1 [RecFlag]
	| RecAll
	deriving Show

decode1 :: RecFlag -> Analyzer BS.ByteString Asn1
decode1 rf = decodeTag >>= \t@(Asn1Tag _ dc _) ->
	Asn1 t <$> (decodeLength >>= decodeCnt dc rf)

decodeTag :: Analyzer BS.ByteString Asn1Tag
decodeTag = decodeTag1 >>= \(tc, dc, mtn) -> maybe
	(Asn1Tag tc dc <$> decodeTagR0)
	(return . Asn1Tag tc dc . fromIntegral)
	mtn

decodeTag1 :: Analyzer BS.ByteString
	(TagClass, DataClass, Maybe Word8)
decodeTag1 = flip fmap token $ \w -> let
	tc = case w `shiftR` 6 of
		0 -> Universal
		1 -> Application
		2 -> ContextSpecific
		3 -> Private
		_ -> error "never occur"
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

decodeCnt :: DataClass -> RecFlag ->
	Maybe Integer -> Analyzer BS.ByteString Asn1Data
decodeCnt Constructed RecAll (Just l) = tokens l >>= \d ->
	case runAnalyzer (listAll $ decode1 RecAll) d of
		Right (r, "") -> return $ Asn1DataAsn1 r
		Left e -> fail e
		_ -> error "never occur"
decodeCnt Constructed (Rec1 f) (Just l) = tokens l >>= \d ->
	case runAnalyzer (listMap decode1 f) d of
		Right (r, "") -> return $ Asn1DataAsn1 r
		Left e -> fail e
		_ -> error "never occur"
decodeCnt _ _ (Just ln) = Asn1DataRaw <$> tokens ln
decodeCnt Constructed (Rec1 rfs) _ = Asn1DataAsn1 <$>
	mapWhileM (/= endOfContents) decode1 rfs
decodeCnt Constructed rf _ = Asn1DataAsn1 <$>
	loopWhileM (/= endOfContents) (decode1 rf)
decodeCnt _ _ _ = fail "Primitive needs length"

endOfContents :: Asn1
endOfContents = Asn1
	(Asn1Tag Universal Primitive 0) (Asn1DataRaw "")

loopWhileM :: Monad m => (a -> Bool) -> m a -> m [a]
loopWhileM p m = m >>= \x -> if p x
	then (x :) `liftM` loopWhileM p m
	else return [x]

mapWhileM :: Monad m =>
	(b -> Bool) -> (a -> m b) -> [a] -> m [b]
mapWhileM _ _ [] = return []
mapWhileM p m (x : xs) = m x >>= \y -> if p y
	then (y :) `liftM` mapWhileM p m xs
	else return [y]
