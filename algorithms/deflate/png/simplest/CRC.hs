{-# LANGUAGE TupleSections #-}

module CRC (check, calc) where

import Control.Arrow (first, second, (&&&))
import Data.List (unfoldr)
import Data.Array (Array, (!), listArray)
import Data.Bits (Bits, complement, xor, shiftR, testBit)
import Data.Bool (bool)
import Data.Word (Word8, Word32)
import qualified Data.ByteString as BS (ByteString, pack, append, foldl')

check :: BS.ByteString -> Word32 -> Bool
check = curry $ (== 0x2144df1c) . calc . uncurry BS.append . second w2bs
	where w2bs = (BS.pack .) . flip curry (4 :: Int) . unfoldr $ \(i, n) ->
		bool Nothing (Just . second (i - 1 ,) $ popByte n) (i > 0)

calc :: BS.ByteString -> Word32
calc = complement . BS.foldl' st 0xffffffff
	where st n b = uncurry xor . first ((table !) . (`xor` b)) $ popByte n

popByte :: (Integral a, Bits a) => a -> (Word8, a)
popByte = fromIntegral &&& (`shiftR` 8)

table :: Array Word8 Word32
table = listArray (0, 255) $ map crc8 [0 .. 255]
	where
	crc8 = (!! 8) . iterate crc1
	crc1 = uncurry (bool id (`xor` 0xedb88320)) . popBit
	popBit = (`testBit` 0) &&& (`shiftR` 1)
