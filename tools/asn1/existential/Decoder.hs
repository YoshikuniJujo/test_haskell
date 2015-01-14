{-# LANGUAGE
	OverloadedStrings,
	ExistentialQuantification,
	DeriveDataTypeable #-}

module Decoder (
	runAnalyzer,
	BerDecode(..), BerDecodeBox(..), getBerDecode,
	Asn1Tag(..), TagClass(..), DataClass(..),
	RawBytes(..), Raw(..), RawConstructed(..),
	Rule(..), RuleType, decodeWith) where

import Control.Applicative
import Control.Monad
import Data.Typeable
import Data.Maybe
import Data.List
import Data.Bits

import qualified Data.ByteString as BS

import Analyzer
import BasicDecoder
import BasicEncoder

class BerDecode a where
	getAsn1Tag :: a -> Asn1Tag
	decodeRule :: a -> Rule

data Rule = Rule { runRule :: RuleType }

type RuleType = [Rule] -> Asn1Tag -> Maybe Integer ->
	Maybe (Analyzer BS.ByteString BerDecodeBox)

decodeWith :: [Rule] -> Analyzer BS.ByteString BerDecodeBox
decodeWith rl = do
	t <- decodeTag
	l <- decodeLength
	fromJust . fromJust . find isJust $
		map (($ l) . ($ t) . ($ rl) . runRule) rl

data BerDecodeBox = forall a .
	(Typeable a, BerDecode a) => BerDecodeBox a
	deriving Typeable

instance BerDecode BerDecodeBox where
	getAsn1Tag (BerDecodeBox a) = getAsn1Tag a
	decodeRule (BerDecodeBox a) = decodeRule a

getBerDecode :: Typeable a => BerDecodeBox -> Maybe a
getBerDecode (BerDecodeBox a) = cast a

------------------------------------------------------------

data Raw = Raw Asn1Tag BS.ByteString
	deriving (Show, Typeable)

instance BerDecode Raw where
	getAsn1Tag (Raw t _) = t
	decodeRule _ = Rule rawRule

rawRule :: RuleType
rawRule _ t (Just l) =
	Just $ BerDecodeBox . Raw t <$> tokens l
rawRule _ _ _ = Just $ fail "Raw needs length"

data RawBytes = RawBytes BS.ByteString
	deriving (Show, Typeable)

instance BerDecode RawBytes where
	getAsn1Tag (RawBytes bs)
		| Right (t, _) <-
			runAnalyzer decodeTag bs = t
		| otherwise = error "Bad RawBytes"
	decodeRule _ = Rule rawBytesRule

rawBytesRule :: RuleType
rawBytesRule _ t (Just l) =
	Just $ BerDecodeBox . RawBytes <$> do
		bs <- tokens l
		return $ encodeTag t
			`BS.append` encodeLength 0 (Just .
				fromIntegral $ BS.length bs)
			`BS.append` bs
rawBytesRule _ _ _ = Just $ fail "RawBytes needs length"

data RawConstructed = RawConstructed Asn1Tag [BerDecodeBox]
	deriving Typeable

instance Show RawConstructed where
	showsPrec d (RawConstructed t _) =
		showParen (d > 10) $
			showString "RawConstructed " .
			showsPrec 11 t . showString " [...]"

instance BerDecode RawConstructed where
	getAsn1Tag (RawConstructed t _) = t
	decodeRule _ = Rule rcRule

rcRule :: RuleType
rcRule _ (Asn1Tag Universal Primitive 0) (Just l)
	| l /= 0 = Just $ fail "Bad end-of-contents"
rcRule _ (Asn1Tag Universal Primitive 0) Nothing =
	Just $ fail "Bad end-of-contents"
rcRule r t@(Asn1Tag _ Constructed _) (Just l) = Just $ do
	s <- tokens l
	let eas = runAnalyzer (listAll $ decodeWith r) s
	case eas of
		Left em -> fail em
		Right (as, "") -> return .
			BerDecodeBox $ RawConstructed t as
		_ -> error "never occur"
rcRule r t@(Asn1Tag _ Constructed _) _ = Just $ do
	as <- loopWhileM notEndOfContents $ decodeWith r
	return . BerDecodeBox $ RawConstructed t as
rcRule _ _ _ = Nothing

loopWhileM :: Monad m => (a -> Bool) -> m a -> m [a]
loopWhileM p m = m >>= \x -> if p x
	then (x :) `liftM` loopWhileM p m
	else return []

notEndOfContents :: BerDecodeBox -> Bool
notEndOfContents =
	(/= Asn1Tag Universal Primitive 0) . getAsn1Tag

------------------------------------------------------------

instance BerDecode a => BerDecode [a] where
	getAsn1Tag _ = Asn1Tag Universal Constructed 16
	decodeRule _ = Rule sequenceRule

sequenceRule :: RuleType
sequenceRule rl t@(Asn1Tag Universal Constructed 16) ln =
	Just $ do
		Just (RawConstructed _ s) <- getBerDecode <$>
			fromJust (rcRule rl t ln)
		return $ BerDecodeBox s
sequenceRule _ _ _ = Nothing

instance BerDecode Bool where
	getAsn1Tag _ = Asn1Tag Universal Primitive 1
	decodeRule _ = Rule boolRule

boolRule :: RuleType
boolRule rl t@(Asn1Tag Universal Primitive 1) ln@(Just 1) =
	Just $ do
		Just (Raw _ bs) <- getBerDecode <$>
			fromJust (rawRule rl t ln)
		return . BerDecodeBox $ bs /= "\x00"
boolRule _ _ _ = Nothing

instance BerDecode Integer where
	getAsn1Tag _ = Asn1Tag Universal Primitive 2
	decodeRule _ = Rule integerRule

integerRule :: RuleType
integerRule r t@(Asn1Tag Universal Primitive 2)
	ln@(Just _) = Just $ do
		Just (Raw _ bs) <- getBerDecode <$>
			fromJust (rawRule r t ln)
		return . BerDecodeBox $ readInteger bs
integerRule _ _ _ = Nothing

------------------------------------------------------------

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
