{-# LANGUAGE TupleSections #-}

module CRC (check, calc) where

import Control.Arrow (first, second)
import Data.Array (Array, (!), listArray)
import Data.Bool (bool)
import Data.Word (Word8, Word32)
import qualified Data.ByteString as BS (ByteString, append, foldl')

import Bits (complement, xor, leToByteString, popBit, popByte)

check :: BS.ByteString -> Word32 -> Bool
check = curry $
	(== 0x2144df1c) . calc . uncurry BS.append . second (leToByteString 4)

calc :: BS.ByteString -> Word32
calc = complement . BS.foldl' st 0xffffffff
	where st n b = uncurry xor . first ((table !) . (`xor` b)) $ popByte n

table :: Array Word8 Word32
table = listArray (0, 255) $ map crc8 [0 .. 255]
	where
	crc8 = (!! 8) . iterate crc1
	crc1 = uncurry (bool id (`xor` 0xedb88320)) . popBit
