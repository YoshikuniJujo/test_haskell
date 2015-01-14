{-# LANGUAGE
	OverloadedStrings,
	ExistentialQuantification #-}

module Encoder () where

import Data.Typeable
import Data.Bits

import qualified Data.ByteString as BS

import BasicEncoder

type TypeNumber = Integer

data Sel
	= SelPrimitive
	| SelConstructed (Maybe Int) [Selector]

data Selector = Selector (TypeNumber -> Integer -> Sel)

class BerEncode b where
	encode :: Selector -> b -> BS.ByteString

data BerEncodeBox =
	forall b . (Typeable b, BerEncode b) => BerEncodeBox b

instance BerEncode BerEncodeBox where
	encode s (BerEncodeBox b) = encode s b

instance BerEncode Bool where
	encode _ b = encodeTag
			(Asn1Tag Universal Primitive 1)
		`BS.append` encodeLength 0 (Just 1)
		`BS.append` (if b then "\xff" else "\x00")

instance BerEncode Integer where
	encode _ n = encodeTag
			(Asn1Tag Universal Primitive 2)
		`BS.append` encodeLength 0
			(Just . fromIntegral $ BS.length bs)
		`BS.append` bs
		where
		bs = integerToBS n

integerToBS :: Integer -> BS.ByteString
integerToBS n = BS.pack $ if testBit b 7 then 0 : s else s
	where
	s@(b : _) | 0 <- n = [0] | otherwise =
		reverse $ integerToWord8s n

instance BerEncode b => BerEncode [b] where
	encode (Selector sel) cs = encodeTag
			(Asn1Tag Universal Constructed 16)
		`BS.append` case sel 16 undefined of
			SelConstructed (Just n) sels ->
				encodeSequenceD n sels cs
			SelConstructed _ sels ->
				encodeSequenceU sels cs
			_ -> error "Bad selector"

encodeSequenceD :: BerEncode b =>
	Int -> [Selector] -> [b] -> BS.ByteString
encodeSequenceD n sels cs = encodeLength n
	(Just . fromIntegral $ BS.length bs) `BS.append` bs
	where
	bs = BS.concat $ zipWith encode sels cs

encodeSequenceU :: BerEncode b =>
	[Selector] -> [b] -> BS.ByteString
encodeSequenceU sels cs = encodeLength 0 Nothing
	`BS.append` bs `BS.append` "\x00\x00"
	where
	bs = BS.concat $ zipWith encode sels cs

testSel, testSel2 :: Selector
testSel = Selector $ \t _ -> case t of
	1 -> SelPrimitive
	2 -> SelPrimitive
	16 -> SelConstructed (Just 0) $ repeat testSel
	_ -> error "not yet defined"
testSel2 = Selector $ \t _ -> case t of
	1 -> SelPrimitive
	2 -> SelPrimitive
	16 -> SelConstructed Nothing $ repeat testSel2
	_ -> error "not yet defined"
