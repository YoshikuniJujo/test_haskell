{-# LANGUAGE
	OverloadedStrings,
	ExistentialQuantification,
	DeriveDataTypeable #-}

module Ber (
	runAnalyzer,
	Ber(..), DerBox(..), getDer,
	Asn1Tag(..), TagClass(..), DataClass(..),
	RawBytes(..), Raw(..), RawConstructed(..),
	Rule(..), RuleType, decodeWith, encodeDer) where

import Control.Applicative
import Control.Monad
import Data.Typeable
import Data.Maybe
import Data.List
import Data.Bits

import qualified Data.ByteString as BS

import Analyzer
import BasicEnDecoder

class Ber a where
	getAsn1Tag :: a -> Asn1Tag
	decodeRule :: a -> Rule

class Ber d => Der d where
	derRule :: d -> BS.ByteString

data Rule = Rule { runRule :: RuleType }

type RuleType = [Rule] -> Asn1Tag -> Maybe Integer ->
	Maybe (Analyzer BS.ByteString DerBox)

decodeWith :: [Rule] -> Analyzer BS.ByteString DerBox
decodeWith rl = do
	t <- decodeTag
	l <- decodeLength
	fromJust . fromJust . find isJust $
		map (($ l) . ($ t) . ($ rl) . runRule) rl

encodeDer :: Der a => a -> BS.ByteString
encodeDer a = encodeTag (getAsn1Tag a)
	`BS.append` encodeLength (Just . fromIntegral $ BS.length bs)
	`BS.append` bs
	where
	bs = derRule a

data DerBox =
	forall a . (Typeable a, Ber a, Der a) => DerBox a
	deriving Typeable

instance Ber DerBox where
	getAsn1Tag (DerBox a) = getAsn1Tag a
	decodeRule (DerBox a) = decodeRule a

instance Der DerBox where
	derRule (DerBox a) = derRule a

getDer :: Typeable a => DerBox -> Maybe a
getDer (DerBox a) = cast a

------------------------------------------------------------

data RawBytes = RawBytes BS.ByteString
	deriving (Show, Typeable)

data Raw = Raw Asn1Tag BS.ByteString
	deriving (Show, Typeable)

data RawConstructed = RawConstructed Asn1Tag [DerBox]
	deriving Typeable

instance Show RawConstructed where
	showsPrec d (RawConstructed t _) =
		showParen (d > 10) $
			showString "RawConstructed " .
			showsPrec 11 t . showString " [...]"

instance Ber RawBytes where
	getAsn1Tag (RawBytes bs) = let
		Right (t, _) = runAnalyzer decodeTag bs in t
	decodeRule _ = Rule rawBytesRule

instance Der RawBytes where
	derRule (RawBytes rb) = let
		Right (_, bs) = runAnalyzer (decodeTag >> decodeLength) rb in
		bs

rawBytesRule :: RuleType
rawBytesRule _ t (Just l) =
	Just $ DerBox . RawBytes <$> do
		bs <- tokens l
		return $ encodeTag t
			`BS.append` encodeLength
				(Just . fromIntegral $ BS.length bs)
			`BS.append` bs
rawBytesRule _ _ _ = Just $ fail "RawBytes needs length"

instance Ber Raw where
	getAsn1Tag (Raw t _) = t
	decodeRule _ = Rule rawRule

instance Der Raw where
	derRule (Raw _ bs) = bs

rawRule :: RuleType
rawRule _ t (Just l) =
	Just $ DerBox . Raw t <$> tokens l
rawRule _ _ _ = Just $ fail "Raw needs length"

instance Ber RawConstructed where
	getAsn1Tag (RawConstructed t _) = t
	decodeRule _ = Rule recRule

instance Der RawConstructed where
	derRule (RawConstructed _ as) = BS.concat $ map encodeDer as

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
			DerBox $ RawConstructed t as
		_ -> error "never occur"
recRule r t@(Asn1Tag _ Constructed _) _ = Just $ do
	as <- loopWhileM notEndOfContents $ decodeWith r
	return . DerBox $ RawConstructed t as
recRule _ _ _ = fail "Primitive needs length"

loopWhileM :: Monad m => (a -> Bool) -> m a -> m [a]
loopWhileM p m = m >>= \x -> if p x
	then (x :) `liftM` loopWhileM p m
	else return []

notEndOfContents :: DerBox -> Bool
notEndOfContents =
	(/= Asn1Tag Universal Primitive 0) . getAsn1Tag

------------------------------------------------------------

instance Ber a => Ber [a] where
	getAsn1Tag _ = Asn1Tag Universal Constructed 16
	decodeRule _ = Rule sequenceRule

instance Der a => Der [a] where
	derRule as = BS.concat $ map encodeDer as

sequenceRule :: RuleType
sequenceRule rl t@(Asn1Tag Universal Constructed 16) ln =
	Just $ do
		Just (RawConstructed _ s) <- getDer <$>
			fromJust (recRule rl t ln)
		return $ DerBox s
sequenceRule _ _ _ = Nothing

instance Ber Bool where
	getAsn1Tag _ = Asn1Tag Universal Primitive 1
	decodeRule _ = Rule boolRule

instance Der Bool where
	derRule b = if b then "\xff" else "\x00"

boolRule :: RuleType
boolRule rl t@(Asn1Tag Universal Primitive 1) ln@(Just 1) =
	Just $ do
		Just (Raw _ bs) <- getDer <$>
			fromJust (rawRule rl t ln)
		return . DerBox $ bs /= "\x00"
boolRule _ _ _ = Nothing

instance Ber Integer where
	getAsn1Tag _ = Asn1Tag Universal Primitive 2
	decodeRule _ = Rule integerRule

instance Der Integer where
	derRule n = BS.pack $ if testBit b 7 then 0 : s else s
		where
		s@(b : _)	| 0 <- n = [0]
				| otherwise = reverse $ integerToWord8s n

integerRule :: RuleType
integerRule r t@(Asn1Tag Universal Primitive 2)
	ln@(Just _) = Just $ do
		Just (Raw _ bs) <- getDer <$>
			fromJust (rawRule r t ln)
		return . DerBox $ readInteger bs
integerRule _ _ _ = Nothing
