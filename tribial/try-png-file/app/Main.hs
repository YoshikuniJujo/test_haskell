{-# LANGUAGE ImportQualifiedPost, PackageImports #-}
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
import Control.Monad
import "mtl" Control.Monad.Except
import Data.Bits
import Data.ByteString qualified as BS
import System.IO.Unsafe
import System.Environment

import Chunks.Core qualified as Chunks
import Crc
import ReadPng

import Tools

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

num32ToBs :: (Bits n, Integral n) => n -> BS.ByteString
num32ToBs num = BS.pack $ fromIntegral <$> [
	num `shiftR` 24,
	num `shiftR` 16 .&. 0xff,
	num `shiftR` 8 .&. 0xff,
	num .&. 0xff ]

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
