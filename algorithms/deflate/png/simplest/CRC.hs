{-# LANGUAGE TupleSections #-}

module CRC (check, calc) where

import Control.Applicative ((<$>))
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
calc = complement . BS.foldl' (flip st) 0xffffffff
	where st b = uncurry xor . first ((table !) . (`xor` b)) . popByte

table :: Array Word8 Word32
table = listArray (0, 255) . (<$> [0 .. 255]) $ (!! 8)
	. iterate (uncurry (bool id (`xor` 0xedb88320)) . popBit)
