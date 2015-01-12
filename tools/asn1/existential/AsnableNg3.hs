{-# LANGUAGE
	OverloadedStrings,
	ExistentialQuantification,
	DeriveDataTypeable #-}

module AsnableNg3 (
	runAnalyzer,
	Asnable(..), AsnableBox(..), getAsnable,
	Asn1Tag(..), TagClass(..), DataClass(..),
	RawBytes(..), Raw(..), RawConstructed(..),
	Rule(..), RuleType, decodeWith ) where

import Control.Applicative
import Control.Monad
import Data.Typeable
import Data.Maybe
import Data.List
import Data.Bits

import qualified Data.ByteString as BS

import Analyzer
import BasicDecoder

class Asnable a where
	getAsn1Tag :: a -> Asn1Tag
	decodeRule :: a -> Rule
	encodeDer :: a -> BS.ByteString

data AsnableBox =
	forall a . (Typeable a, Asnable a) => AsnableBox a
	deriving Typeable

instance Asnable AsnableBox where
	getAsn1Tag (AsnableBox a) = getAsn1Tag a
	decodeRule (AsnableBox a) = decodeRule a
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
	decodeRule _ = Rule rawBytesRule
	encodeDer (RawBytes bs) = bs

instance Asnable Raw where
	getAsn1Tag (Raw t _) = t
	decodeRule _ = Rule rawRule
	encodeDer (Raw t bs) = encodeTag t
		`BS.append` encodeLength
			(Just . fromIntegral $ BS.length bs)
		`BS.append` bs

instance Asnable RawConstructed where
	getAsn1Tag (RawConstructed t _) = t
	decodeRule _ = Rule recRule
	encodeDer (RawConstructed t as) = encodeTag t
		`BS.append` encodeLength
			(Just . fromIntegral $ BS.length bs)
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
recRule r t@(Asn1Tag _ Constructed _) (Just l) = Just $ do
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

instance Asnable a => Asnable [a] where
	getAsn1Tag _ = Asn1Tag Universal Constructed 16
	decodeRule _ = Rule sequenceRule
	encodeDer as = encodeTag (getAsn1Tag as)
		`BS.append` encodeLength
			(Just . fromIntegral $ BS.length bs)
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
	decodeRule _ = Rule boolRule
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
	decodeRule _ = Rule integerRule
	encodeDer n = encodeTag (getAsn1Tag n)
		`BS.append` encodeLength
			(Just . fromIntegral $ BS.length s')
		`BS.append` s'
		where
		s' = BS.pack $
			if testBit b 7 then 0 : s else s
		s@(b : _)	| 0 <- n = [0]
				| otherwise = reverse $ integerToWord8s n

integerRule :: RuleType
integerRule r t@(Asn1Tag Universal Primitive 2)
	ln@(Just _) = Just $ do
		Just (Raw _ bs) <- getAsnable <$>
			fromJust (rawRule r t ln)
		return . AsnableBox $ readInteger bs
integerRule _ _ _ = Nothing
