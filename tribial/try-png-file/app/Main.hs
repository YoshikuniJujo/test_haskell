{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Foreign.Ptr
import Foreign.Concurrent
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Storable
import Control.Arrow
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Data.Array
import Data.Bits
import Data.Bool
import Data.Word
import Data.ByteString qualified as BS
import System.IO.Unsafe
import System.Environment

import Chunks.Core qualified as Chunks

main :: IO ()
main = do
	fp : _ <- getArgs
	png <- BS.readFile fp
--	let	c = fst $ (checkMagic >> chunk) `runReadPng` png
	let	c = fst $ readChunks `runReadPng` png
	print $ fst' <$> c
	print $ take 100 . show . snd' <$> c
	print $ third <$> c
--	print @(Either String (Chunk Chunks.Ihdr)) c
--	print @(Either String (Chunk BS.ByteString)) c
	print $ fromChunk . fst' <$> c
--	print $ fromChunk . snd' <$> c

fst' :: (a, b, c) -> a
fst' (x, _, _) = x

snd' :: (a, b, c) -> b
snd' (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

magic :: BS.ByteString
magic = "\x89\x50\x4e\x47\x0d\x0a\x1a\x0a"

readChunks :: ReadPng (Chunk Chunks.Ihdr, Chunk BS.ByteString, Chunk BS.ByteString)
readChunks = do
	checkMagic
	(,,) <$> chunk <*> chunk <*> chunk

data Chunk a = Chunk { chunkName :: BS.ByteString, chunkData :: a }
	deriving Show

class Chunkable a where
	fromByteString :: BS.ByteString -> a; toByteString :: a -> BS.ByteString

instance Chunkable BS.ByteString where
	fromByteString = id; toByteString = id

instance Chunkable Chunks.Ihdr where
	fromByteString bs = unsafePerformIO $ BS.useAsCStringLen bs \(pbs, pln) -> do
		p <- malloc
		copyBytes (castPtr p) pbs (min pln $ sizeOf (undefined :: Chunks.Ihdr))
		Chunks.Ihdr_ <$> newForeignPtr p (free p)
	toByteString (Chunks.Ihdr_ fp) = unsafePerformIO $ withForeignPtr fp \p ->
		BS.packCStringLen (castPtr p, sizeOf (undefined :: Chunks.Ihdr))

bsToNum32 :: (Bits n, Integral n) => BS.ByteString -> n
bsToNum32 bs
	| BS.length bs == 4 = bigEndian 0 . (fromIntegral <$>) $ BS.unpack bs
	| otherwise = error "bad"

num32ToBs :: (Bits n, Integral n) => n -> BS.ByteString
num32ToBs num = BS.pack $ fromIntegral <$> [
	num `shiftR` 24,
	num `shiftR` 16 .&. 0xff,
	num `shiftR` 8 .&. 0xff,
	num .&. 0xff ]

bigEndian :: Bits n => n -> [n] -> n
bigEndian s [] = s
bigEndian s (n : ns) = bigEndian (s `shiftL` 8 .|. n) ns

type ReadPng = ExceptT String (State BS.ByteString)

runReadPng :: ReadPng a -> BS.ByteString -> (Either String a, BS.ByteString)
runReadPng = runState . runExceptT

pop :: Int -> ReadPng BS.ByteString
pop n = lift $ gets (BS.take n) <* modify (BS.drop n)

checkMagic :: ReadPng ()
checkMagic = do
	m <- pop 8
	when (m /= magic) $ throwError "not PNG file"

dataLength :: ReadPng Int
dataLength = bsToNum32 <$> pop 4

getChunkName :: ReadPng BS.ByteString
getChunkName = pop 4

chunk :: Chunkable a => ReadPng (Chunk a)
chunk = do
	ln <- dataLength
	nm <- getChunkName
	dt <- pop ln
	c <- pop 4
	when (not . check (nm `BS.append` dt) $ bsToNum32 c) $ throwError "bad CRC"
	pure Chunk { chunkName = nm, chunkData = fromByteString dt }

fromChunk :: Chunkable a => Chunk a -> BS.ByteString
fromChunk Chunk { chunkName = nm, chunkData = dt } =
	num32ToBs (BS.length bs) `BS.append` nm `BS.append`
	bs `BS.append` num32ToBs (crc $ nm `BS.append` bs)
	where
	bs = toByteString dt

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
