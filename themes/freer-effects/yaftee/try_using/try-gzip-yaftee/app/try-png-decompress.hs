{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Arrow
import Control.Monad
import Control.Monad.ToolsYj
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.IO qualified as PipeIO
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeBS
import Control.Monad.Yaftee.Pipe.ByteString.OnDemand qualified as OnDemand
import Control.Monad.Yaftee.Pipe.ByteString.Crc qualified as Crc
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.Fail qualified as Fail
import Control.Monad.Yaftee.IO qualified as IO
import Control.HigherOpenUnion qualified as U
import Data.Foldable
import Data.Bits
import Data.Bits.ToolsYj
import Data.Word
import Data.ByteString qualified as BS
import Data.ByteString.ToolsYj qualified as BS
import Data.ByteString.BitArray qualified as BitArray
import System.IO
import System.Environment

import Control.Monad.Yaftee.Pipe.Deflate.Decompress qualified as Deflate

import Pipe.Huffman qualified as Huffman
import Pipe.Runlength qualified as Runlength

main :: IO ()
main = do
	fp : _ <- getArgs
	let	processHeader = IO.print
	h <- openFile fp ReadMode
	void . Eff.runM
		. Crc.runCrc32 @"foobar"
		. OnDemand.run_ @"foobar"
		. OnDemand.run_ @"barbaz"
--		. OnDemand.run_ @"hogepiyo"
		. (`State.run` Chunk "IHDR")
		. (`State.run` (1 :: Word32, 0 :: Word32))
		. Deflate.run_ @"hogepiyo"
		. Except.run @String
		. Fail.runExc id
		. Pipe.run
		$ PipeBS.hGet 64 h Pipe.=$=
			OnDemand.onDemand "foobar" Pipe.=$=
			PipeT.checkRight Pipe.=$= Crc.crc32 "foobar" Pipe.=$= do
				State.putN "foobar" $ OnDemand.RequestBytes 8
				IO.print =<< Pipe.await
				doWhile_ $ chunk1 "foobar" 10
			Pipe.=$= do
				Left (ChunkBegin "IHDR") <- Pipe.await
				PipeT.checkRight Pipe.=$= OnDemand.onDemand "barbaz" Pipe.=$= (readHeader "barbaz" processHeader `Except.catch` IO.print @String)
				Left (ChunkEnd "IHDR") <- Pipe.await

				IO.print "IHDR end"

				chunkToByteString

--				forever $ Pipe.yield =<< Pipe.await
			Pipe.=$= do

				IO.print =<< Pipe.isMore

				IO.print @Chunk =<< State.get

--				bs <- Pipe.await
				bs <- untilIdat
				IO.print =<< Pipe.isMore

				IO.print @Chunk =<< State.get

				OnDemand.onDemandWithInitial "hogepiyo" bs Pipe.=$= do
					zlib

				IO.print =<< State.get @Chunk
				IO.print . uncurry (.|.) . second (`shiftL` 16) =<< State.get @(Word32, Word32)
				forever $ Pipe.yield =<< Pipe.await
				IO.print =<< State.get @Chunk
				
			Pipe.=$= do
				PipeIO.print'
				IO.print @(Word32, Word32) =<< State.get

zlib :: (
	U.Member Pipe.P es,
	Deflate.Members "hogepiyo" es,
	U.Member (State.Named "hogepiyo" PipeBS.Length) es,
	U.Member (State.Named "hogepiyo" Crc.Crc32) es,
	OnDemand.Members "hogepiyo" es,
	Runlength.Members "hogepiyo" es,
	U.Member (State.Named "hogepiyo" Huffman.ExtraBits) es,
	U.Member (State.Named "hogepiyo" (Huffman.BinTreePair Int)) es,
	U.Member (State.S (Word32, Word32)) es,
	U.Member (Except.E String) es,
	U.Member Fail.F es,
	U.Base IO.I es ) =>
	Eff.E es (Either BitArray.B BS.ByteString) BS.ByteString ()
zlib = do
					State.putN "hogepiyo" $ OnDemand.RequestBytes 1
					h1 <- BS.toBits <$> (Except.getRight @String "not Right" =<< Pipe.await)
					IO.print @Word8 $ h1 `shiftR` 4
					IO.print @Word8 $ h1 .&. 0xf
					h2 <- BS.toBits <$> (Except.getRight @String "not Right" =<< Pipe.await)
					IO.print @Word8 $ h2 `shiftR` 6
					IO.print @Word8 $ (h2 `shiftR` 5) .&. 1
					IO.print @Word32 $ ((fromIntegral h1 `shiftL` 8) .|. fromIntegral h2) `mod` 31
					(Deflate.decompress "hogepiyo" 65 `Except.catch` IO.print @String) Pipe.=$= adler32'
--					Deflate.decompress "hogepiyo" Pipe.=$= adler32'
					State.putN "hogepiyo" $ OnDemand.RequestBytes 4
					IO.print @Word32 . BS.toBitsBE =<< skipLeft1

skipLeft1 :: (
	Show a,
	U.Member Pipe.P es,
	U.Base IO.I es
	) =>
	Eff.E es (Either a b) o b
skipLeft1 = Pipe.await >>= \case
	Left a -> do
		IO.print a
		Pipe.await >>= \case
			Left b -> do
				IO.print b
				error "bad"
			Right c -> pure c
	Right b -> pure b

untilIdat :: (
	U.Member Pipe.P es,
	U.Member (State.S Chunk) es,
	U.Base IO.I es
	) =>
	Eff.E es BS.ByteString o BS.ByteString
untilIdat = do
	IO.print =<< Pipe.isMore
	bs <- Pipe.await
	State.get >>= \case
		Chunk "IDAT" -> do
			IO.print "BEGIN IDAT"
			pure bs
		c -> do	IO.print c
			IO.print bs
			untilIdat

data Header = Header {
	headerWidth :: Word32,
	headerHeight :: Word32,
	headerBitDepth :: Word8,
	headerColorType :: ColorType,
	headerCompressionMethod :: CompressionMethod,
	headerFilterMethod :: FilterMethod,
	headerInterlaceMethod :: InterlaceMethod }
	deriving Show

newtype ColorType = ColorType Word8 deriving (Eq, Bits)

pattern ColorTypePalletUsed, ColorTypeColorUsed :: ColorType
pattern ColorTypePalletUsed = ColorType 1
pattern ColorTypeColorUsed = ColorType 2
pattern ColorTypeAlphaChannelUsed = ColorType 4

instance Show ColorType where
	show ColorTypePalletUsed  = "ColorTypePaletteUsed"
	show ColorTypeColorUsed = "ColorTypeColorUsed"
	show ColorTypeAlphaChannelUsed = "ColorTypeAlphaChannelUsed"
	show (ColorType n) = "(ColorType " ++ show n ++ ")"

newtype CompressionMethod = CompressionMethod Word8 deriving (Eq, Bits)

pattern CompressionMethodDeflate = CompressionMethod 0

instance Show CompressionMethod where
	show CompressionMethodDeflate = "CompressionMethodDeflate"
	show (CompressionMethod n) = "(CompressionMethod " ++ show n ++ ")"

newtype FilterMethod = FilterMethod Word8 deriving (Eq, Bits)

pattern FilterMethodDefaultFilter :: FilterMethod
pattern FilterMethodDefaultFilter = FilterMethod 0

instance Show FilterMethod where
	show FilterMethodDefaultFilter = "FilterMethodDefaultFilter"
	show (FilterMethod n) = "(FilterMethod " ++ show n ++ ")"

newtype InterlaceMethod = InterlaceMethod Word8 deriving (Eq, Bits)

pattern InterlaceMethodNon, InterlaceMethodAdam7 :: InterlaceMethod
pattern InterlaceMethodNon = InterlaceMethod 0
pattern InterlaceMethodAdam7 = InterlaceMethod 1

instance Show InterlaceMethod where
	show InterlaceMethodNon = "InterlaceMethodNon"
	show InterlaceMethodAdam7 = "InterlaceMethodAdam7"
	show (InterlaceMethod n) = "(InterlaceMethod " ++ show n ++ ")"

readHeader :: forall nm -> (
	U.Member Pipe.P es,
	OnDemand.Members nm es,
	U.Member (Except.E String) es,
	U.Member Fail.F es
	) =>
	(Header -> Eff.E es (Either x BS.ByteString) o ()) ->
		Eff.E es (Either x BS.ByteString) o ()
readHeader nm proc = do
	State.putN nm $ OnDemand.RequestBytes 4
	w <- BS.toBitsBE <$> (Except.getRight "not right" =<< Pipe.await)
	h <- BS.toBitsBE <$> (Except.getRight "not right" =<< Pipe.await)
	State.putN nm $ OnDemand.RequestBytes 1
	bd <- BS.toBitsBE <$> (Except.getRight "not right" =<< Pipe.await)
	State.putN nm $ OnDemand.RequestBytes 1
	ct <- BS.toBitsBE <$> (Except.getRight "not right" =<< Pipe.await)
	State.putN nm $ OnDemand.RequestBytes 1
	cm <- BS.toBitsBE <$> (Except.getRight "not right" =<< Pipe.await)
	State.putN nm $ OnDemand.RequestBytes 1
	fm <- BS.toBitsBE <$> (Except.getRight "not right" =<< Pipe.await)
	State.putN nm $ OnDemand.RequestBytes 1
	im <- BS.toBitsBE <$> (Except.getRight "not right" =<< Pipe.await)
	proc Header {
		headerWidth = w, headerHeight = h,
		headerBitDepth = bd,
		headerColorType = ct,
		headerCompressionMethod = cm,
		headerFilterMethod = fm,
		headerInterlaceMethod = im }

chunkToByteString :: (
	U.Member Pipe.P es,
	U.Member (State.S Chunk) es,
	U.Member (Except.E String) es,
	U.Member Fail.F es
	) =>
	Eff.E es (Either ChunkTag o) o ()
chunkToByteString = do
	Left (ChunkBegin c) <- Pipe.await
	case c of
		"IEND" -> do
			Left (ChunkEnd "IEND") <- Pipe.await
			pure ()
		_ -> do	State.put $ Chunk c
			getUntilChunkEnd
			chunkToByteString

getUntilChunkEnd :: (
	U.Member Pipe.P es,
	U.Member (Except.E String) es
	) =>
	Eff.E es (Either ChunkTag o) o ()
getUntilChunkEnd = Pipe.await >>= \case
	Left (ChunkEnd c) -> do
		-- check chunk name
		pure ()
	Right bs -> Pipe.yield bs >> getUntilChunkEnd
	_ -> Except.throw @String "bad"

chunk1 :: forall nm -> (
	U.Member Pipe.P es,
	OnDemand.Members nm es,
	U.Member (State.Named nm Crc.Crc32) es,
	U.Member (Except.E String) es ) =>
	Int -> Eff.E es BS.ByteString (Either ChunkTag BS.ByteString) Bool
chunk1 nm m = do
	State.putN nm $ OnDemand.RequestBytes 4
	n <- be <$> Pipe.await

	Crc.resetCrc32 nm
	State.putN nm (OnDemand.RequestBytes 4)
	cn <- Pipe.await
	Pipe.yield . Left $ ChunkBegin cn

	for_ (split m n) \n' -> do
		State.putN nm $ OnDemand.RequestBytes n'
		Pipe.yield =<< Right <$> Pipe.await

	Crc.compCrc32 nm

	crc1 <- State.getN nm

	State.putN nm $ OnDemand.RequestBytes 4
	crc0 <- Crc.byteStringToCrc32BE <$> Pipe.await

	when (Just crc1 /= crc0) $ Except.throw "chunk1: CRC32 error"

	Pipe.yield . Left $ ChunkEnd cn

	pure $ cn /= "IEND"

newtype Chunk = Chunk BS.ByteString deriving Show
data ChunkTag = ChunkBegin BS.ByteString | ChunkEnd BS.ByteString deriving Show

be :: Bits n => BS.ByteString -> n
be = BS.foldl (\s b -> s `shiftL` 8 .|. bitsToBits 8 b) zeroBits

split :: Int -> Int -> [Int]
split n 0 = []
split n m
	| n < m = n : split n (m - n)
	| otherwise = [m]

adler32Step :: (Word32, Word32) -> BS.ByteString -> (Word32, Word32)
adler32Step = BS.foldl \(a, b) w ->
	((a + fromIntegral w) `mod` 65521, (b + a + fromIntegral w) `mod` 65521)

adler32 :: (
	U.Member Pipe.P es,
	U.Member (State.S (Word32, Word32)) es ) =>
	Eff.E es BS.ByteString BS.ByteString r
adler32 = forever do
	bs <- Pipe.await
	State.modify (`adler32Step` bs)
	Pipe.yield bs

adler32' :: (
	U.Member Pipe.P es,
	U.Member (State.S (Word32, Word32)) es ) =>
	Eff.E es BS.ByteString BS.ByteString ()
adler32' = Pipe.awaitMaybe >>= \case
	Nothing -> pure ()
	Just bs -> do
		State.modify (`adler32Step` bs)
		Pipe.yield bs
		adler32'
