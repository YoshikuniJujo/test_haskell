{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Crc (

	crc, step', check

	) where

import Control.Arrow
import Data.Bits
import Data.Array
import Data.Bool
import Data.Word
import Data.ByteString qualified as BS

popBit :: Bits a => a -> (Bool, a)
popBit n = (n `testBit` 0, n `shiftR` 1)

crc1 :: Word32 -> Word32
crc1 = uncurry (bool id (`xor` 0xedb88320)) . popBit

crc8 :: Word8 -> Word32
crc8 n = iterate crc1 (fromIntegral n) !! 8

table :: Array Word8 Word32
table = listArray (0, 255) $ map crc8 [0 .. 255]

popByte :: (Integral a, Bits a) => a -> (Word8, a)
popByte n = (fromIntegral n, n `shiftR` 8)

step :: Word32 -> Word8 -> Word32
step n b = uncurry xor . (first $ (table !) . (`xor` b)) $ popByte n

step' :: Word32 -> BS.ByteString -> Word32
step' n = BS.foldl' step n

crc :: BS.ByteString -> Word32
crc = complement . BS.foldl' step 0xffffffff

word32ToBytes :: Word32 -> [Word8]
word32ToBytes = wtl (4 :: Int)
	where
	wtl i | i < 1 = const []
	wtl i = uncurry (:) . (wtl (i - 1) `second`) . popByte

check :: BS.ByteString -> Word32 -> Bool
check bs n = crc (bs `BS.append` (BS.pack $ word32ToBytes n)) == 0x2144df1c
