{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import GHC.TypeLits
import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.IO qualified as PipeIO
import Control.Monad.Yaftee.Pipe.ByteString.Lazy qualified as PipeLBS
import Control.Monad.Yaftee.Pipe.ByteString.Lazy.OnDemand qualified as OnDemand
import Control.Monad.Yaftee.Pipe.ByteString.Lazy.Adler32 qualified as Adler32
import Control.Monad.Yaftee.Pipe.Png.Decode.Header qualified as Header
import Control.Monad.Yaftee.Pipe.Png.Decode.Chunk qualified as Chunk
import Control.Monad.Yaftee.Pipe.Deflate.Decompress qualified as Deflate
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.Fail qualified as Fail
import Control.Monad.Yaftee.IO qualified as IO
import Control.HigherOpenUnion qualified as U
import Data.Word
import Data.Int
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.ToolsYj qualified as LBS
import Data.ByteString.Lazy.BitArray qualified as BitArray
import Data.Png
import Data.Png.Header qualified as Header
import Data.Zlib qualified as Zlib
import System.IO
import System.Environment

import Pipe.Runlength qualified as Runlength
import Pipe.Huffman qualified as Huffman

main :: IO ()
main = do
	fp : _ <- getArgs
	h <- openFile fp ReadMode
	void . Eff.runM
		. Except.run @String . Fail.runExc id
		. Adler32.run_ @"foobar"
		. Runlength.run_ @_ @"foobar"
		. Huffman.run @"foobar" @Int
		. flip (State.runN @"foobar") Header.header0
		. flip (State.runN @"foobar") (ByteString "")
		. OnDemand.run_ @"foobar"
		. Chunk.chunkRun_ @"foobar"
		. Pipe.run . (`Except.catch` IO.putStrLn) . void $
		PipeLBS.hGet 64 h Pipe.=$= do
			readFileHeader "foobar"
			Chunk.chunk "foobar" 50
		Pipe.=$= do
			OnDemand.onDemand "foobar" Pipe.=$= Header.read "foobar" IO.print
			bs <- untilIdat "foobar"
			OnDemand.onDemandWithInitial "foobar" bs Pipe.=$= do
				State.putN "foobar" $ OnDemand.RequestBytes 2
				IO.print =<< zlibHeader =<< Except.getRight @String "bad" =<< Pipe.await
				State.putN "foobar" $ OnDemand.RequestBuffer 100
				rs <- Header.headerToRows <$> State.getN "foobar"
				IO.print rs
				void $ Deflate.decompress "foobar"
--					Pipe.=$= (format "foobar" ((+ 1) <$> rs) >> Pipe.await)
					Pipe.=$= PipeT.convert (either (LBS.pack . (: [])) id)
					Pipe.=$= Adler32.adler32 "foobar"
	--			IO.print @ByteString =<< State.getN "foobar"
				State.putN "foobar" . OnDemand.RequestPushBack . BitArray.fromByteString . unByteString =<< State.getN "foobar"
				Right "" <- Pipe.await
				ad1 <- Adler32.toWord32 <$> State.getN "foobar"
				State.putN "foobar" $ OnDemand.RequestBytes 4
				ad0 <- LBS.toBitsBE <$> PipeT.skipLeft1
				when (ad1 /= ad0) $ Except.throw @String "Adler-32 error"
		Pipe.=$= PipeIO.print

format :: forall (nm :: Symbol) -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm ByteString) es ) =>
	[Int64] -> Eff.E es (Either Word8 LBS.ByteString) LBS.ByteString ()
format nm = \case
	[] -> pure ()
	n : ns -> (Pipe.yield =<< readBytes nm n) >> format nm ns
	
readBytes :: forall (nm :: Symbol) -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm ByteString) es ) =>
	Int64 -> Eff.E es (Either Word8 LBS.ByteString) o LBS.ByteString
readBytes nm n = State.getsN nm (LBS.splitAt' n . unByteString) >>= \case
	Nothing -> readMore nm >> readBytes nm n
	Just (t, d) -> t <$ State.putN nm (ByteString d)

readMore :: forall (nm :: Symbol) -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm ByteString) es ) =>
	Eff.E es (Either Word8 LBS.ByteString) o ()
readMore nm = Pipe.await >>= either
	(\w -> State.modifyN nm $ ByteString . (`LBS.snoc` w) . unByteString)
	(\s -> State.modifyN nm $ ByteString . (`LBS.append` s) . unByteString)

newtype ByteString = ByteString { unByteString :: LBS.ByteString } deriving Show

readFileHeader :: forall (nm :: Symbol) -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm Chunk.ByteString) es,
	U.Member (State.Named nm Chunk.Crc32) es,
	U.Member (Except.E String) es
	) =>
	Eff.E es LBS.ByteString o ()
readFileHeader nm = do
	ph <- Chunk.readBytes nm 8
	when (ph /= fileHeader) $ Except.throw @String "PNG File header error"

zlibHeader :: (U.Member (Except.E String) es, U.Member Fail.F es) =>
	LBS.ByteString -> Eff.E es i o Zlib.Header
zlibHeader bs = do
	[cmf, flg] <- pure $ LBS.unpack bs
	maybe (Except.throw @String "Zlib header check bits error")
		pure (Zlib.readHeader cmf flg)

untilIdat :: forall nm -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm Chunk.Chunk) es
	) =>
	Eff.E es LBS.ByteString o LBS.ByteString
untilIdat nm = do
	bs <- Pipe.await
	State.getN nm >>= \case
		Chunk.Chunk "IDAT" -> do
			pure bs
		c -> do
			untilIdat nm
