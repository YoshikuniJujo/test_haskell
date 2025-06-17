{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.ByteString.Sequence where

import Prelude hiding (concat, length, replicate)
import Prelude qualified as P
import GHC.Exts
import Control.Monad
import Data.Foldable qualified as F
import Data.Semigroup
import Data.List.NonEmpty qualified as NE
import Data.Bool
import Data.Char
import Data.Word
import Data.Sequence qualified as Seq
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC

newtype ByteString = ByteString { unByteString :: Seq.Seq BS.ByteString }

instance Show ByteString where
	showsPrec  p ps r = showsPrec p (unpackChars ps) r

instance Read ByteString where
	readsPrec p str = [ (packChars x, y) | (x, y) <- readsPrec p str ]

instance Eq ByteString where (==) = eq

instance Ord ByteString where
	ByteString b1 `compare` ByteString b2 = b1 `compare` b2

eq :: ByteString -> ByteString -> Bool
eq (ByteString b1) (ByteString b2) = b1 == b2

instance IsString ByteString where fromString = packChars

instance Semigroup ByteString where
	(<>) = append
	sconcat (b NE.:| bs) = concat (b : bs)
	stimes n = ByteString
		. join . Seq.replicate (fromIntegral n) . unByteString

instance Monoid ByteString where
	mempty = empty
	mappend = (<>)
	mconcat = concat

instance IsList ByteString where
	type Item ByteString = Word8
	fromList = packBytes
	toList = unpackBytes

normalize :: ByteString -> ByteString
normalize = ByteString . Seq.filter (not . BS.null) . unByteString

empty :: ByteString
empty = ByteString Seq.empty

singleton :: Word8 -> ByteString
singleton = ByteString . Seq.singleton . BS.singleton

pack :: [Word8] -> ByteString
pack = ByteString . Seq.singleton . BS.pack

unpack :: ByteString -> [Word8]
unpack = F.concat . (BS.unpack <$>) . toList . unByteString

cons :: Word8 -> ByteString -> ByteString
cons w (ByteString bs) = ByteString $ BS.singleton w Seq.:<| bs

snoc :: ByteString -> Word8 -> ByteString
snoc (ByteString bs) w = ByteString $ bs Seq.:|> BS.singleton w

append :: ByteString -> ByteString -> ByteString
append (ByteString b1) (ByteString b2) = ByteString $ b1 Seq.>< b2

uncons :: ByteString -> Maybe (Word8, ByteString)
uncons (ByteString bsa) = case bsa of
	Seq.Empty -> Nothing
	bs Seq.:<| bss -> case BS.uncons bs of
		Nothing -> uncons $ ByteString bss
		Just (b, bs') -> Just (
			b,
			ByteString $ bool (bs' Seq.:<| bss) bss (BS.null bs') )

unsnoc :: ByteString -> Maybe (ByteString, Word8)
unsnoc (ByteString bsa) = case bsa of
	Seq.Empty -> Nothing
	bss Seq.:|> bs -> case BS.unsnoc bs of
		Nothing -> unsnoc $ ByteString bss
		Just (bs', b) -> Just (
			ByteString $ bool (bss Seq.:|> bs') bss (BS.null bs'),
			b )

null :: ByteString -> Bool
null (ByteString Seq.Empty) = True
null _ = False

length :: ByteString -> Int
length = sum . (BS.length <$>) . unByteString

concat :: [ByteString] -> ByteString
concat = ByteString . join . Seq.fromList . (unByteString <$>)

replicate :: Int -> Word8 -> ByteString
replicate n = pack . P.replicate n

{-
splitAt' :: Int -> ByteString -> Maybe (ByteString, ByteString)
splitAt' n bs
	| length bs < n = Nothing
	| otherwise = 
	-}

packChars :: String -> ByteString
packChars = ByteString . Seq.singleton . BSC.pack

unpackChars :: ByteString -> String
unpackChars = F.concat
	. ((chr . fromIntegral <$>) . BS.unpack <$>) . F.toList . unByteString

packBytes :: [Word8] -> ByteString
packBytes = ByteString . Seq.singleton . BS.pack

unpackBytes :: ByteString -> [Word8]
unpackBytes = F.concat . (BS.unpack <$>) . F.toList . unByteString
