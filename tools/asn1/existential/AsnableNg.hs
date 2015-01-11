{-# LANGUAGE
	OverloadedStrings,
	ExistentialQuantification,
	DeriveDataTypeable #-}

module AsnableNg (
	runAnalyzer,
	Asnable(..), AsnableBox(..), getAsnable,
	Asn1Tag(..), TagClass(..), DataClass(..),
	RawBytes(..), Raw(..), RawConstructed(..),
	Rule(..), RuleType, decodeWith, rawBytesRule, rawRule, recRule,
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
	encodeDer :: a -> BS.ByteString

data AsnableBox =
	forall a . (Typeable a, Asnable a) => AsnableBox a
	deriving Typeable

instance Asnable AsnableBox where
	getAsn1Tag (AsnableBox a) = getAsn1Tag a
	encodeDer (AsnableBox a) = encodeDer a

getAsnable :: Typeable a => AsnableBox -> Maybe a
getAsnable (AsnableBox a) = cast a

data RawBytes = RawBytes BS.ByteString
	deriving (Show, Typeable)

data Raw = Raw Asn1Tag BS.ByteString
	deriving (Show, Typeable)

data RawConstructed = RawConstructed Asn1Tag [AsnableBox]
	deriving Typeable

instance Show RawConstructed where
	showsPrec d (RawConstructed t _) =
		showParen (d > 10) $
			showString "RawConstructed " .
			showsPrec 11 t . showString " [...]"

instance Asnable RawBytes where
	getAsn1Tag (RawBytes bs) = let
		Right (t, _) = runAnalyzer decodeTag bs in t
	encodeDer (RawBytes bs) = bs

instance Asnable Raw where
	getAsn1Tag (Raw t _) = t
	encodeDer (Raw t bs) = encodeTag t
		`BS.append` encodeLength (Just . fromIntegral $ BS.length bs)
		`BS.append` bs

instance Asnable RawConstructed where
	getAsn1Tag (RawConstructed t _) = t
	encodeDer (RawConstructed t as) = encodeTag t
		`BS.append` encodeLength (Just . fromIntegral $ BS.length bs)
		`BS.append` bs
		where
		bs = BS.concat $ map encodeDer as

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

rawBytesRule :: RuleType
rawBytesRule _ t (Just l) =
	Just $ AsnableBox . RawBytes <$> do
		bs <- tokens l
		return $ encodeTag t
			`BS.append` encodeLength
				(Just . fromIntegral $ BS.length bs)
			`BS.append` bs
rawBytesRule _ _ _ = Just $ fail "RawBytes needs length"

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
	deriving (Show, Eq, Enum)

data DataClass
	= Primitive
	| Constructed
	deriving (Show, Eq, Enum)

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

------------------------------------------------------------

encodeTag :: Asn1Tag -> BS.ByteString
encodeTag (Asn1Tag tc dc n)
	| n < 31 = BS.pack [c .|. fromIntegral n]
	| otherwise = BS.pack $ (c .|. 0x1f) : encodeTagR n
	where
	c = fromIntegral $ fromEnum tc `shiftL` 6 .|.
		fromEnum dc `shiftL` 5

encodeTagR :: Integer -> [Word8]
encodeTagR n = map (0x80 .|.) (reverse ws) ++ [w]
	where
	w : ws = integerToWord7s n

integerToWord7s :: Integer -> [Word8]
integerToWord7s 0 = []
integerToWord7s n = fromIntegral (n .&. 0x7f) :
	integerToWord7s (n `shiftR` 7)

encodeLength :: Maybe Integer -> BS.ByteString
encodeLength (Just n)
	| n < 0 = error "No negative length"
	| n < 128 = BS.pack [fromIntegral n]
	| otherwise = BS.pack $
		(0x80 .|. fromIntegral (length ws)) : ws
	where
	ws = reverse $ integerToWord8s n
encodeLength _ = "\x80"

integerToWord8s :: Integer -> [Word8]
integerToWord8s 0 = []
integerToWord8s n = fromIntegral (n .&. 0xff) :
	integerToWord8s (n `shiftR` 8)

------------------------------------------------------------

instance Asnable a => Asnable [a] where
	getAsn1Tag _ = Asn1Tag Universal Constructed 16
	encodeDer as = encodeTag (getAsn1Tag as)
		`BS.append` encodeLength (Just . fromIntegral $ BS.length bs)
		`BS.append` bs
		where
		bs = BS.concat $ map encodeDer as

sequenceRule :: RuleType
sequenceRule rl t@(Asn1Tag Universal Constructed 16) ln =
	Just $ do
		Just (RawConstructed _ s) <- getAsnable <$>
			fromJust (recRule rl t ln)
		return $ AsnableBox s
sequenceRule _ _ _ = Nothing

instance Asnable Bool where
	getAsn1Tag _ = Asn1Tag Universal Primitive 1
	encodeDer b = encodeTag (getAsn1Tag b)
		`BS.append` encodeLength (Just 1)
		`BS.append` (if b then "\xff" else "\x00")

boolRule :: RuleType
boolRule rl t@(Asn1Tag Universal Primitive 1) ln@(Just 1) =
	Just $ do
		Just (Raw _ bs) <- getAsnable <$>
			fromJust (rawRule rl t ln)
		return . AsnableBox $ bs /= "\x00"
boolRule _ _ _ = Nothing

instance Asnable Integer where
	getAsn1Tag _ = Asn1Tag Universal Primitive 2
	encodeDer n = encodeTag (getAsn1Tag n)
		`BS.append` encodeLength
			(Just . fromIntegral $ BS.length bs')
		`BS.append` bs'
		where
		bs' = BS.pack $ if testBit b 7 then 0 : bs else bs
		bs@(b : _)	| 0 <- n = [0]
				| otherwise = reverse $ integerToWord8s n

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
