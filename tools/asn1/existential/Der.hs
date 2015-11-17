{-# LANGUAGE
	OverloadedStrings,
	ExistentialQuantification #-}

module Der (
	runAnalyzer,
	Ber(..), BerBox(..), getBer,
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
import BasicDecoder
import Ber

class Ber d => Der d where
	derRule :: d -> BS.ByteString

encodeDer :: Der a => a -> BS.ByteString
encodeDer a = encodeTag (getAsn1Tag a)
	`BS.append` encodeLength (Just . fromIntegral $ BS.length bs)
	`BS.append` bs
	where
	bs = derRule a

------------------------------------------------------------

instance Der RawBytes where
	derRule (RawBytes rb) = let
		Right (_, bs) = runAnalyzer (decodeTag >> decodeLength) rb in
		bs

rawBytesRule :: RuleType
rawBytesRule _ t (Just l) =
	Just $ BerBox . RawBytes <$> do
		bs <- tokens l
		return $ encodeTag t
			`BS.append` encodeLength
				(Just . fromIntegral $ BS.length bs)
			`BS.append` bs
rawBytesRule _ _ _ = Just $ fail "RawBytes needs length"

instance Der Raw where
	derRule (Raw _ bs) = bs

rawRule :: RuleType
rawRule _ t (Just l) =
	Just $ BerBox . Raw t <$> tokens l
rawRule _ _ _ = Just $ fail "Raw needs length"

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
			BerBox $ RawConstructed t as
		_ -> error "never occur"
recRule r t@(Asn1Tag _ Constructed _) _ = Just $ do
	as <- loopWhileM notEndOfContents $ decodeWith r
	return . BerBox $ RawConstructed t as
recRule _ _ _ = fail "Primitive needs length"

loopWhileM :: Monad m => (a -> Bool) -> m a -> m [a]
loopWhileM p m = m >>= \x -> if p x
	then (x :) `liftM` loopWhileM p m
	else return []

notEndOfContents :: BerBox -> Bool
notEndOfContents =
	(/= Asn1Tag Universal Primitive 0) . getAsn1Tag

------------------------------------------------------------

instance Der a => Der [a] where
	derRule as = BS.concat $ map encodeDer as

instance Der Bool where
	derRule b = if b then "\xff" else "\x00"

instance Der Integer where
	derRule n = BS.pack $ if testBit b 7 then 0 : s else s
		where
		s@(b : _)	| 0 <- n = [0]
				| otherwise = reverse $ integerToWord8s n
