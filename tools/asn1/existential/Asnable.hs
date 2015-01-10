{-# LANGUAGE
	OverloadedStrings,
	ExistentialQuantification,
	DeriveDataTypeable #-}

module Asnable (
	runAnalyzer,
	Asnable(..), AsnableBox(..), getAsnable,
	Asn1Tag(..), TagClass(..), DataClass(..),
	Raw(..), RawConstructed(..),
	Rule(..), RuleType, decodeWith, rawRule, recRule,
	sequenceRule, boolRule, integerRule,
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
	showsPrec d (RawConstructed t _) =
		showParen (d > 10) $
			showString "RawConstructed " .
			showsPrec 11 t . showString " [...]"

instance Asnable Raw where
	getAsn1Tag (Raw t _) = t

instance Asnable RawConstructed where
	getAsn1Tag (RawConstructed t _) = t

------------------------------------------------------------

data Rule = Rule { runRule :: RuleType }

type RuleType = [Rule] -> Asn1Tag -> Maybe Integer ->
	Maybe (Analyzer BS.ByteString AsnableBox)

decodeWith :: [Rule] -> Analyzer BS.ByteString AsnableBox
decodeWith rl = do
	t <- decodeTag
	l <- decodeLength
	fromJust . fromJust . find isJust $
		map (($ l) . ($ t) . ($ rl) . runRule) rl

rawRule :: RuleType
rawRule _ t (Just l) =
	Just $ AsnableBox . Raw t <$> tokens l
rawRule _ _ _ = Just $ fail "Raw needs length"

recRule :: RuleType
recRule _ (Asn1Tag Universal Primitive 0) (Just l)
	| l /= 0 = fail "Bad end-of-contents"
recRule _ (Asn1Tag Universal Primitive 0) Nothing =
	fail "Bad end-of-contents"
recRule _ t@(Asn1Tag _ Primitive _) (Just l) =
	Just $ AsnableBox . Raw t <$> tokens l
recRule r t (Just l) = Just $ do
	s <- tokens l
	let eas = runAnalyzer (listAll $ decodeWith r) s
	case eas of
		Left em -> fail em
		Right (as, "") -> return .
			AsnableBox $ RawConstructed t as
		_ -> error "never occur"
recRule r t@(Asn1Tag _ Constructed _) _ = Just $ do
	as <- loopWhileM notEndOfContents $ decodeWith r
	return . AsnableBox $ RawConstructed t as
recRule _ _ _ = fail "Primitive needs length"

loopWhileM :: Monad m => (a -> Bool) -> m a -> m [a]
loopWhileM p m = m >>= \x -> if p x
	then (x :) `liftM` loopWhileM p m
	else return []

notEndOfContents :: AsnableBox -> Bool
notEndOfContents =
	(/= Asn1Tag Universal Primitive 0) . getAsn1Tag

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

sequenceRule :: RuleType
sequenceRule rl t@(Asn1Tag Universal Constructed 16) ln =
	Just $ do
		Just (RawConstructed _ s) <- getAsnable <$>
			fromJust (recRule rl t ln)
		return $ AsnableBox s
sequenceRule _ _ _ = Nothing

instance Asnable Bool where
	getAsn1Tag _ = Asn1Tag Universal Primitive 1

boolRule :: RuleType
boolRule rl t@(Asn1Tag Universal Primitive 1) ln@(Just 1) =
	Just $ do
		Just (Raw _ bs) <- getAsnable <$>
			fromJust (rawRule rl t ln)
		return . AsnableBox $ bs /= "\x00"
boolRule _ _ _ = Nothing

instance Asnable Integer where
	getAsn1Tag _ = Asn1Tag Universal Primitive 2

integerRule :: RuleType
integerRule r t@(Asn1Tag Universal Primitive 2)
	ln@(Just _) = Just $ do
		Just (Raw _ bs) <- getAsnable <$>
			fromJust (rawRule r t ln)
		return . AsnableBox $ readInteger bs
integerRule _ _ _ = Nothing

readInteger :: BS.ByteString -> Integer
readInteger bs = case BS.uncons bs of
	Just (h, t) -> if testBit h 7
		then readIntegerR
			(fromIntegral h - 0x100) t
		else readIntegerR (fromIntegral h) t
	_ -> 0

readIntegerR :: Integer -> BS.ByteString -> Integer
readIntegerR n bs = case BS.uncons bs of
	Just (h, t) -> readIntegerR
		(n `shiftL` 8 .|. fromIntegral h) t
	_ -> n
