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
import System.IO
import System.Environment

main :: IO ()
main = do
	fp : _ <- getArgs
	let	processHeader = IO.print
	h <- openFile fp ReadMode
	void . Eff.runM
		. Crc.runCrc32 @"foobar"
		. OnDemand.run_ @"foobar"
		. OnDemand.run_ @"barbaz"
		. OnDemand.run_ @"hogepiyo"
		. (`State.run` Chunk "IHDR")
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
--				PipeT.checkRight Pipe.=$= OnDemand.onDemand "barbaz" Pipe.=$= readHeader "barbaz" processHeader
--				Left (ChunkEnd "IHDR") <- Pipe.await

				Pipe.yield "foobarbaz"

				chunkToByteString

--				forever $ Pipe.yield =<< Pipe.await
			Pipe.=$= do

				IO.print =<< Pipe.isMore

{-
				OnDemand.onDemand "hogepiyo" Pipe.=$= do
					State.putN "hogepiyo" $ OnDemand.RequestBytes 1
					IO.print =<< Pipe.await
					-}

				forever $ Pipe.yield =<< Pipe.await

			Pipe.=$= PipeIO.print

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
