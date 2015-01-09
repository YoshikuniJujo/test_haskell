{-# LANGUAGE OverloadedStrings, ExistentialQuantification, DeriveDataTypeable #-}

module Asnable (
	runAnalyzer,
	Asnable(..), Asn1Tag(..), TagClass(..), DataClass(..),
	AsnableBox(..), getAsnable, Raw(..), RawConstructed(..),
	Rule(..), decodeSel, rawSel, recSel, sequenceSel,
) where

import Control.Applicative
import Control.Monad
import Data.Typeable
import Data.Maybe
import Data.List
import Data.Bits
import Data.Word8

import qualified Data.ByteString as BS

import Analyzer

class Asnable a where
	getAsn1Tag :: a -> Asn1Tag

data AsnableBox =
	forall a . (Typeable a, Asnable a) => AsnableBox a
	deriving Typeable

instance Asnable AsnableBox where
	getAsn1Tag (AsnableBox a) = getAsn1Tag a

getAsnable :: Typeable a => AsnableBox -> Maybe a
getAsnable (AsnableBox a) = cast a

data Raw = Raw Asn1Tag BS.ByteString
	deriving (Show, Typeable)

data RawConstructed = RawConstructed Asn1Tag [AsnableBox]
	deriving Typeable

instance Show RawConstructed where
	show (RawConstructed t _) =
		"RawConstructed " ++ show t ++ " [...]"

instance Asnable Raw where
	getAsn1Tag (Raw t _) = t

instance Asnable RawConstructed where
	getAsn1Tag (RawConstructed t _) = t

------------------------------------------------------------

runRule :: Rule -> RuleType
runRule (Rule r) = r

data Rule = Rule RuleType

type RuleType = [Rule] ->
	Asn1Tag -> Maybe Integer -> Maybe (Analyzer BS.ByteString AsnableBox)

decodeSel :: [Rule] -> Analyzer BS.ByteString AsnableBox
decodeSel sel = do
	t <- decodeTag
	l <- decodeLength
	fromJust . fromJust . find isJust $
		map (($ l) . ($ t) . ($ sel) . runRule) sel

rawSel :: RuleType
rawSel _ t (Just l) = Just $ AsnableBox . Raw t <$> tokens l
rawSel _ _ _ = Just $ fail "Raw needs length"

recSel :: RuleType
recSel _ (Asn1Tag Universal Primitive 0) (Just l)
	| l /= 0 = fail "Bad end-of-contents"
recSel _ (Asn1Tag Universal Primitive 0) Nothing =
	fail "Bad end-of-contents"
recSel _ t@(Asn1Tag _ Primitive _) (Just l) = Just $ AsnableBox . Raw t <$> tokens l
recSel r t (Just l) = Just $ do
	s <- tokens l
	let eas = runAnalyzer (listAll $ decodeSel r) s
	case eas of
		Left em -> fail em
		Right (as, "") -> return . AsnableBox $ RawConstructed t as
		_ -> error "never occur"
recSel r t@(Asn1Tag _ Constructed _) _ = Just $ do
	as <- loopWhileM notEndOfContents $ decodeSel r
	return . AsnableBox $ RawConstructed t as
recSel _ _ _ = fail "Primitive needs length"

loopWhileM :: Monad m => (a -> Bool) -> m a -> m [a]
loopWhileM p m = m >>= \x -> if p x
	then (x :) `liftM` loopWhileM p m
	else return []

notEndOfContents :: AsnableBox -> Bool
notEndOfContents = (/= Asn1Tag Universal Primitive 0) . getAsn1Tag

------------------------------------------------------------

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

------------------------------------------------------------

instance Asnable [a] where
	getAsn1Tag _ = Asn1Tag Universal Constructed 16

sequenceSel :: RuleType
sequenceSel r t@(Asn1Tag Universal Constructed 16) ln@(Just _) = Just $ do
	rc <- fromJust $ recSel r t ln
	let Just (RawConstructed _ as) = getAsnable rc
	return $ AsnableBox as
sequenceSel _ _ _ = Nothing
