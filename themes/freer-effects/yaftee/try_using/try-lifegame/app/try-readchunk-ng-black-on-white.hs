{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Arrow
import Control.Monad
import Control.Monad.ToolsYj
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeBS
import Control.Monad.Yaftee.Pipe.BytesCrc32 qualified as Bytes
import Control.Monad.Yaftee.Pipe.PngNg.Decode.Chunk
import Control.Monad.Yaftee.Pipe.Png.Encode.Chunk qualified as Chunk
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.Fail qualified as Fail
import Control.Monad.Yaftee.IO qualified as IO
import Control.HigherOpenUnion qualified as U
import Data.List
import Data.Ratio
import Data.Word
import Data.Word.Word8 qualified as Word8
import Data.ByteString.FingerTree qualified as BSF
import Data.ByteString.FingerTree.Bits qualified as BSF
import Data.Png.Header.Data qualified as Header
import Data.Apng qualified as Apng
import System.IO
import System.Environment
import System.Directory
import System.FilePath

import Tools

import Control.Monad.Yaftee.Pipe.Png.Palette qualified as Palette
import Data.Vector qualified as V

main :: IO ()
main = do
	dr : d_ : de_ : fpo : _ <- getArgs

	fps@(fp : _) <- ((dr </>) <$>) . sort . filter (not . ("." `isSuffixOf`)) <$> getDirectoryContents dr

	let	-- fps = take 500 fps_
		n = length fps
		d = read d_ :: Ratio Word16
		de = read de_ :: Ratio Word16

	hh <- openFile fp ReadMode
	((), Just (wdt, hgt)) <- Eff.runM
		. (flip (State.run @_ @(Maybe (Word32, Word32))) Nothing)
		. Bytes.bytesRun_ @"foobar"
		. flip (State.runN @"foobar") ("" :: BSF.ByteString)
		. Except.run @String . Pipe.run
		. (`Except.catch` IO.putStrLn)
		. void $ PipeBS.hGet 32 hh
		Pipe.=$= PipeT.convert BSF.fromStrict
		Pipe.=$= chunks "foobar"
		Pipe.=$= do
			IO.print =<< Pipe.await
			State.put =<< headerToSize <$> chunkBody "foobar"
	hClose hh

--	hs <- (`openFile` ReadMode) `mapM` fps
	ho <- openFile fpo WriteMode
	void . Eff.runM
		. Bytes.bytesRun_ @"foobar"
		. flip (State.runN @"foobar") ("" :: BSF.ByteString)
		. flip (State.runN @"foobar") (replicate (n - 1) d ++ [de])
		. Except.run @String . Fail.run . Pipe.run
		. (`Fail.catch` IO.putStrLn) . (`Except.catch` IO.putStrLn)
		. void $ ((\fp -> Eff.effBase (openFile fp ReadMode) >>= \h -> PipeBS.hGet 32 h >> Eff.effBase (hClose h)) `mapM_` fps)
		Pipe.=$= PipeT.convert BSF.fromStrict
		Pipe.=$= replicateM_ n (chunks "foobar")
		Pipe.=$= do
			Pipe.yield \sn -> (Chunk.Chunk "IHDR"
				. BSF.fromStrict
				. Header.encodeHeader $ header wdt hgt, sn)
			Pipe.yield \sn -> (Chunk.Chunk "PLTE"
				$ Palette.encodePalette blackOnWhite, sn)
			Pipe.yield \sn -> (Chunk.Chunk "acTL"
				. Apng.encodeActl . actl $ fromIntegral n, sn)

			do	
				ChunkBegin "IHDR" <- Pipe.await
				_bd <- chunkBody "foobar"
				Just d' <- pop "foobar"
				Pipe.yield \sn -> (
					Chunk.Chunk "fcTL"
						(Apng.encodeFctl' sn $ fctl wdt hgt d'), -- $ read d),
					sn + 1 )
				doWhile_ do
					ChunkBegin cnm <- Pipe.await
					case cnm of
						"IDAT" -> do
							bd <- chunkBody "foobar"
							Pipe.yield \sn -> (Chunk.Chunk cnm bd, sn)
							pure True
						"IEND" -> do
							ChunkEnd <- Pipe.await
							pure False
						_ -> pure True

			doWhile_ $ Pipe.awaitMaybe >>= \case
				Nothing -> pure False
				Just (ChunkBegin "IHDR") -> do	
					_bd <- chunkBody "foobar"
					Just d' <- pop "foobar"
					Pipe.yield \sn -> (
						Chunk.Chunk "fcTL"
							(Apng.encodeFctl' sn $ fctl wdt hgt d'), -- $ read d),
						sn + 1 )
					doWhile_ do
						ChunkBegin cnm <- Pipe.await
						case cnm of
							"IDAT" -> do
								bd <- chunkBody "foobar"
								Pipe.yield \sn -> (fdatChunk sn bd, sn + 1)
								pure True
							"IEND" -> do
								ChunkEnd <- Pipe.await
								pure False
							_ -> pure True
					pure True

			Pipe.yield \sn -> (Chunk.Chunk "IEND" "", sn)

		Pipe.=$= Chunk.chunks 0
		Pipe.=$= PipeT.convert BSF.toStrict
		Pipe.=$= PipeBS.hPutStr ho
--	hClose `mapM_` hs;
	hClose ho

headerToSize :: BSF.ByteString -> Maybe (Word32, Word32)
headerToSize hdr = do
	(w, hdr') <- first Word8.toBitsBE <$> BSF.splitAt' 4 hdr
	(h, _) <- first Word8.toBitsBE <$> BSF.splitAt' 4 hdr'
	pure (w, h)

chunkBody :: forall nm -> (
	U.Member Pipe.P es, U.Member (State.Named nm BSF.ByteString) es,
	U.Member (Except.E String) es ) => Eff.E es Chunk o BSF.ByteString
chunkBody nm = Pipe.await >>= \case
	ChunkBody bd -> State.modifyN nm (<> bd) >> chunkBody nm
	ChunkEnd -> State.getN nm <* State.putN @BSF.ByteString nm ""
	_ -> Except.throw @String "chunkBody: not ChunkBody"

header :: Word32 -> Word32 -> Header.Header
header w h = Header.Header {
	Header.headerWidth = w, Header.headerHeight = h,
	Header.headerBitDepth = 1,
	Header.headerColorType = Header.ColorTypePalette,
	Header.headerCompressionMethod = Header.CompressionMethodDeflate,
	Header.headerFilterMethod = Header.FilterMethodDefaultFilter,
	Header.headerInterlaceMethod = Header.InterlaceMethodNon }

actl :: Word32 -> Apng.Actl
actl fn = Apng.Actl { Apng.actlFrames = fn, Apng.actlPlays = 0 }

fctl :: Word32 -> Word32 -> Ratio Word16 -> Apng.Fctl
fctl w h d = Apng.Fctl {
	Apng.fctlSequenceNumber = 0,
	Apng.fctlWidth = w, Apng.fctlHeight = h,
	Apng.fctlXOffset = 0, Apng.fctlYOffset = 0,
	Apng.fctlDelayNum = numerator d, Apng.fctlDelayDen = denominator d,
	Apng.fctlDisposeOp = Apng.disposeOpNone,
	Apng.fctlBlendOp = Apng.blendOpSource }

fdatChunk :: Word32 -> BSF.ByteString -> Chunk.Chunk
fdatChunk sn bd = Chunk.Chunk "fdAT" $ BSF.fromBitsBE' sn <> bd

blackOnWhite :: Palette.Palette
blackOnWhite = Palette.Palette (V.fromList [(191, 191, 191), (0, 0, 0)])
