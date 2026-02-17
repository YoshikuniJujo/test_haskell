{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.UUIDv7 (

	UUIDv7, nextUUIDv7, fromWords, toWords, fromInts, toInts

	) where

import Foreign.C.Types
import Control.Arrow
import Data.Bits
import Data.Word
import Data.Int
import Data.ByteString qualified as BS
import Data.UnixTime
import System.Entropy
import Numeric

data UUIDv7 = UUIDv7 { upper :: Word64, lower :: Word64 }
	deriving (Eq, Ord)

fromWords :: Word64 -> Word64 -> UUIDv7
fromWords = UUIDv7

toWords :: UUIDv7 -> (Word64, Word64)
toWords = upper &&& lower

fromInts :: Int64 -> Int64 -> UUIDv7
fromInts (fromIntegral -> u) (fromIntegral -> l) =
	UUIDv7 (u1 .|. u2 .|. svn) l'
	where
	u1 = (u .&.	0x0ffffffffffff000) `shiftL` 4
	u2 = u .&.	0x0000000000000fff
	svn =		0x0000000000007000
	l' = l .|.	0x8000000000000000

toInts :: UUIDv7 -> (Int64, Int64)
toInts (UUIDv7 u l) = (fromIntegral $ u1 .|. u2, fromIntegral l')
	where
	u1 = (u .&. 0xffffffffffff0000) `shiftR` 4
	u2 = u .&. 0x0000000000000fff
	l' = l .&. 0x3fffffffffffffff

instance Show UUIDv7 where
	show (UUIDv7 (hx 16 -> u) (hx 16 -> l)) = hyphen [8, 4, 4, 4] $ u ++ l

hx :: Integral n => Int -> n -> String
hx n i = replicate (n - length s) '0' ++ s where s = showHex i ""

hyphen :: [Int] -> String -> String
hyphen [] s = s
hyphen (n : ns) s = take n s ++ "-" ++ hyphen ns (drop n s)

nextUUIDv7 :: IO UUIDv7
nextUUIDv7 = do
	ut <- getUnixTime
	et <- BS.unpack <$> getEntropy 10
	let	u48 = milliseconds ut `shiftL` 16
		m16 = foldToWord64 $ take 2 et
		l64 = foldToWord64 $ drop 2 et
	pure $ UUIDv7 {
		upper = (u48 .|. m16) .&. verOff .|. verOn,
		lower = l64 .&. varOff .|. varOn }

milliseconds :: UnixTime -> Word64
milliseconds ut =
	fromIntegral ((\(CTime ct) -> ct) $ utSeconds ut) * 1000 +
	fromIntegral (utMicroSeconds ut) `div` 1000

foldToWord64 :: [Word8] -> Word64
foldToWord64 = foldl (\a b -> a `shiftL` 8 .|. fromIntegral b) 0

verOff, verOn, varOff, varOn :: Word64
verOff	= 0xffffffffffff0fff
verOn	= 0x0000000000007000
varOff	= 0x3fffffffffffffff
varOn	= 0x8000000000000000
