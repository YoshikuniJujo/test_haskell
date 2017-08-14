{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Numeric
import Control.Arrow
import Control.Monad
import Data.Monoid
import Data.Array
import Data.Bool
import Data.Bits
import Data.Word
import System.IO
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal

import qualified Data.ByteString as BS

checkPng :: FilePath -> IO ()
checkPng fp = do
	h <- openFile fp ReadMode
	header h >>= putStrLn . unwords . map showByte
	readChunk h >>= print
	readChunk h >>= print . \(l, t, _) -> (l, t)
	readChunk h >>= print

header :: Handle -> IO [Word8]
header h = alloca $ \p -> do
	hGetBuf h p 8
	peekArray 8 p

showByte :: Word8 -> String
showByte w = replicate (2 - length s) '0' ++ s
	where s = showHex w ""

readChunk :: Handle -> IO (Word32BE, BS.ByteString, BS.ByteString)
readChunk h = alloca $ \p -> do
	hGetBuf h p 4
	l <- peek p
	t <- BS.hGet h 4
	d <- BS.hGet h $ fromIntegral l
	hGetBuf h p 4
	Word32BE c <- peek p
	print . crc $ t <> d
	print c
	when (not $ check (t <> d) c) $ error "CRC check failed"
	return (l, t, d)

newtype Word32BE = Word32BE Word32
	deriving (Show, Eq, Ord, Enum, Num, Real, Integral)

buildBigEndian :: Num a => [Word8] -> a
buildBigEndian = bbe 0
	where
	bbe n [] = n
	bbe n (w : ws) = bbe (n * 2 ^ 8 + fromIntegral w) ws

destroyBigEndian :: Integral a => a -> [Word8]
destroyBigEndian = dbe []
	where
	dbe ws n | n <= 0 = ws
	dbe ws n = let (d, m) = n `divMod` (2 ^ 8) in
		dbe (fromIntegral m : ws) d

instance Storable Word32BE where
	sizeOf _ = 4
	alignment _ = 4
	peek p = buildBigEndian <$> peekArray 4 (castPtr p)
	poke p (Word32BE n) = pokeArray (castPtr p) $ destroyBigEndian n

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

crc :: BS.ByteString -> Word32
crc = complement . BS.foldl' step 0xffffffff

word32ToBytes :: Word32 -> [Word8]
word32ToBytes = wtl (4 :: Int)
	where
	wtl i | i < 1 = const []
	wtl i = uncurry (:) . (wtl (i - 1) `second`) . popByte

check :: BS.ByteString -> Word32 -> Bool
check bs n = crc (bs `BS.append` (BS.pack $ word32ToBytes n)) == 0x2144df1c
