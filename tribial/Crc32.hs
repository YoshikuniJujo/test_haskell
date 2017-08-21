{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Crc32 (checksum, verify) where

import Control.Arrow
import Data.Monoid
import Data.Array
import Data.Bits
import Data.Bool
import Data.Word

import qualified Data.ByteString as BS

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

checksum :: BS.ByteString -> Word32
checksum = complement . BS.foldl' step 0xffffffff

word32ToBytes :: Word32 -> [Word8]
word32ToBytes = wtl (4 :: Word8)
	where
	wtl i | i < 1 = const []
	wtl i = uncurry (:) . (wtl (i - 1) `second`) . popByte

checksumBS :: BS.ByteString -> BS.ByteString
checksumBS = BS.pack . word32ToBytes . checksum

verify :: BS.ByteString -> Word32 -> Bool
verify bs cs =
	checksumBS (bs <> BS.pack (word32ToBytes cs)) == "\x1c\xdf\x44\x21"
