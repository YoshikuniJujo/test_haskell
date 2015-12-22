import Control.Arrow
import Data.Array
import Data.Bits
import Data.Bool
import Data.Word
import qualified Data.ByteString.Lazy as LBS

check :: LBS.ByteString -> Word32 -> Bool
check bs n = crc (bs `LBS.append` (LBS.pack $ word32ToBytes n)) == 0x2144df1c

crc :: LBS.ByteString -> Word32
crc = complement . LBS.foldl' step 0xffffffff

step :: Word32 -> Word8 -> Word32
step n b = uncurry xor . (first $ (table !) . (`xor` b)) $ popByte n

table :: Array Word8 Word32
table = listArray (0, 255) $ map crc8 [0 .. 255]

crc8 :: Word8 -> Word32
crc8 n = iterate crc1 (fromIntegral n) !! 8

crc1 :: Word32 -> Word32
crc1 = uncurry (bool id (`xor` 0xedb88320)) . popBit

word32ToBytes :: Word32 -> [Word8]
word32ToBytes = wtl (4 :: Int)
	where
	wtl i | i < 1 = const []
	wtl i = uncurry (:) . (wtl (i - 1) `second`) . popByte

popByte :: (Integral a, Bits a) => a -> (Word8, a)
popByte n = (fromIntegral n, n `shiftR` 8)

popBit :: Bits a => a -> (Bool, a)
popBit n = (n `testBit` 0, n `shiftR` 1)
