import Control.Applicative
import Control.Arrow
import Data.List
import Data.Bits
import Data.Word
import Data.Char
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import System.IO
import System.IO.Unsafe
import File.Binary.PNG

import Codec.Compression.Zlib

sample :: [Chunk]
Right sample = unsafePerformIO $
	getChunks <$> (hGetContents =<< openBinaryFile "small.png" ReadMode)

zlibedBSL :: BSL.ByteString
Just (ChunkIDAT (IDAT zlibedBSL)) = find ((== T_IDAT) . typeChunk) sample

zlibedBS :: BS.ByteString
zlibedBS = BSL.toStrict zlibedBSL

zlibed :: [Word8]
zlibed = BS.unpack zlibedBS

plainBS :: BS.ByteString
plainBS = BSL.toStrict $ body sample

plain :: [Word8]
plain = BSL.unpack $ body sample

adler32 :: [Word8] -> [Word8]
adler32 = (\((a1, a0), (b1, b0)) -> [b1, b0, a1, a0])
	. ((fromIntegral *** fromIntegral) . (`divMod` 0x100)
		*** (fromIntegral *** fromIntegral) . (`divMod` 0x100))
	. foldl' (\(a, b) x ->
		let a' = (a + x) `mod` 65521 in (a', (b + a') `mod` 65521)) (1, 0)
	. map fromIntegral

adler32BS :: BS.ByteString -> BS.ByteString
adler32BS = BS.pack
	. (\((a1, a0), (b1, b0)) -> [b1, b0, a1, a0])
	. ((fromIntegral *** fromIntegral) . (`divMod` 0x100)
		*** (fromIntegral *** fromIntegral) . (`divMod` 0x100))
	. BS.foldl' (\(a, b) x ->
		let a' = (a + fromIntegral x) `mod` 65521 in (a', (b + a') `mod` 65521)) (1, 0)
	. BS.map fromIntegral

adler32BSL :: BSL.ByteString -> [Word8]
adler32BSL = (\((a1, a0), (b1, b0)) -> [b1, b0, a1, a0])
	. ((fromIntegral *** fromIntegral) . (`divMod` 0x100)
		*** (fromIntegral *** fromIntegral) . (`divMod` 0x100))
	. BSL.foldl' (\(a, b) x ->
		let a' = (a + fromIntegral x) `mod` 65521 in (a', (b + a') `mod` 65521)) (1, 0)
	. BSL.map fromIntegral

binary :: Char -> String
binary = bn 7 . ord
	where
	bn n _ | n < 0 = ""
	bn n x = (if testBit x n then '1' else '0') : bn (n - 1) x
