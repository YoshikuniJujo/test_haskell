{-# LANGUAGE OverloadedStrings, TypeFamilies #-}

module DecodeAsn1Common (
	runAnalyzer, decode1,
	decodeTag1, decodeTag,
	decodeLength, decodeLength1, decodeLengthR,
	) where

import Control.Applicative
import Control.Monad
import Data.Bits
import Data.Word8

import qualified Data.ByteString as BS

import Analyzer
import qualified ListLike as LL

data Asn1 = Asn1 Asn1Tag Asn1Data deriving (Show, Eq)
data Asn1Tag = Asn1Tag TagClass DataClass Integer
	deriving (Show, Eq)
data TagClass =
	Universal | Application | ContextSpecific | Private
	deriving (Show, Eq)
data DataClass = Primitive | Constructed deriving (Show, Eq)
data Asn1Data
	= Asn1DataAsn1 [Asn1]
	| Asn1DataRaw BS.ByteString
	deriving (Show, Eq)

decode1 :: Analyzer BS.ByteString Asn1
decode1 = do
	t@(Asn1Tag _ dc _) <- decodeTag
	Asn1 t <$> (decodeLength dc >>= decodeContents)

decodeTag :: Analyzer BS.ByteString Asn1Tag
decodeTag = do
	(tc, dc, mtn) <- decodeTag1
	maybe (Asn1Tag tc dc <$> decodeTagR0)
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
				| otherwise ->
					error "never_occur"
	return (tc, dc, tn)

decodeTagR0 :: (LL.ListLike a, LL.Element a ~ Word8) =>
	Analyzer a Integer
decodeTagR0 = do
	n <- decodeTagR 0
	when (n <= 30) $ fail
		"Use single byte for tag number 0 - 30"
	return n

decodeTagR :: (LL.ListLike a, LL.Element a ~ Word8) =>
	Integer -> Analyzer a Integer
decodeTagR 0 = do
	w <- token
	when (w == 0x80) $ fail
		"Redundant byte for tag number"
	if testBit w 7
		then decodeTagR $ fromIntegral (w .&. 0x7f)
		else return $ fromIntegral w
decodeTagR n = do
	w <- token
	if testBit w 7
		then decodeTagR $ n `shiftL` 7
			.|. fromIntegral (w .&. 0x7f)
		else return $ n `shiftL` 7
			.|. fromIntegral w

decodeContents :: Maybe Integer ->
	Analyzer BS.ByteString Asn1Data
decodeContents (Just ln) = do
	dt <- tokens ln
	return $ Asn1DataRaw dt
decodeContents _ = do
	as <- loopWhileM
		(/= Asn1 (Asn1Tag Universal Primitive 0)
			(Asn1DataRaw ""))
		decode1
	return $ Asn1DataAsn1 as

loopWhileM :: Monad m => (a -> Bool) -> m a -> m [a]
loopWhileM p m = do
	x <- m
	if p x	then (x :) `liftM` loopWhileM p m
		else return [x]

decodeLength :: (LL.ListLike a, LL.Element a ~ Word8) =>
	DataClass -> Analyzer a (Maybe Integer)
decodeLength dc = do
	ln1 <- decodeLength1
	case ln1 of
		Just (Right ln) ->
			return . Just $ fromIntegral ln
		Just (Left n) -> Just <$> decodeLengthR 0 n
		_ -> case dc of
			Primitive -> fail
				"Primitive needs length."
			_ -> return Nothing

decodeLength1 :: (LL.ListLike a, LL.Element a ~ Word8) =>
	Analyzer a (Maybe (Either Int Int))
decodeLength1 = do
	w <- token
	return $ if not (testBit w 7)
		then Just . Right $ fromIntegral w
		else let
			ln = w .&. 0x7f in
			if ln == 0
				then Nothing
				else Just . Left $
					fromIntegral ln

decodeLengthR :: (LL.ListLike a, LL.Element a ~ Word8) =>
	Integer -> Int -> Analyzer a Integer
decodeLengthR ln 0 = return ln
decodeLengthR ln n = do
	w <- token
	decodeLengthR
		(ln `shiftL` 8 .|. fromIntegral w)
		(n - 1)
