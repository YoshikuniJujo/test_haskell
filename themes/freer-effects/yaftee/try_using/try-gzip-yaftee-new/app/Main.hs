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
import Control.Arrow
import Control.Monad
import Control.Monad.ToolsYj
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.IO qualified as PipeIO
import Control.Monad.Yaftee.Pipe.ByteString.Lazy qualified as PipeLBS
import Control.Monad.Yaftee.Pipe.ByteString.Lazy.OnDemand qualified as OnDemand
import Control.Monad.Yaftee.Pipe.ByteString.Lazy.Crc qualified as Crc
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.Fail qualified as Fail
import Control.Monad.Yaftee.IO qualified as IO
import Control.HigherOpenUnion qualified as U
import Data.Bits
import Data.Word
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.ToolsYj qualified as LBS
import Data.ByteString.Lazy.BitArray qualified as BitArray
import System.IO
import System.Environment

import Control.Monad.Yaftee.Pipe.Gzip.Decompress
import Pipe.Huffman qualified as Huffman
import Pipe.Runlength qualified as RunLength

import Data.Gzip
import Data.Gzip.Calc

main :: IO ()
main = do
	fp : _ <- getArgs
	h <- openFile fp ReadMode
	void . Eff.runM . Except.run @String . Fail.runExc id

			. RunLength.run_ @_ @"foobar"

			. (`State.run` Huffman.PhaseOthers)
			. (`State.run` Huffman.IsLiteral @Int (const False))

			. (flip (State.runN @"foobar") $ Huffman.BitArray (BitArray.fromByteString ""))
			. Huffman.run @"foobar" @Int

			. OnDemand.run_ @"foobar"
			. Crc.runCrc32 @"foobar"
			. PipeLBS.lengthRun @"foobar"
			. Pipe.run $
		(`Except.catch` IO.putStrLn) . void $ PipeLBS.hGet 64 h Pipe.=$=
			gzipDecompress Pipe.=$= PipeIO.print

gzipDecompress :: (
	U.Member Pipe.P es,

	RunLength.Members "foobar" es,

	U.Member (State.S Huffman.Phase) es,
	U.Member (State.S (Huffman.IsLiteral Int)) es,
	U.Member (State.Named "foobar" (Huffman.BinTreePair Int)) es,
	U.Member (State.Named "foobar" Huffman.BitArray) es,
	U.Member (State.Named "foobar" Huffman.ExtraBits) es,

	U.Member (State.Named "foobar" OnDemand.Request) es,
	U.Member (State.Named "foobar" OnDemand.BitArray) es,
	U.Member (State.Named "foobar" Crc.Crc32) es,
	U.Member (State.Named "foobar" PipeLBS.Length) es,
	U.Member (Except.E String) es,
	U.Member Fail.F es,
	U.Base IO.I es
	) =>
	Eff.E es LBS.ByteString LBS.ByteString ()
gzipDecompress = void $ OnDemand.onDemand "foobar" Pipe.=$= do
	_ <- PipeT.checkRight Pipe.=$= readHeader "foobar" (const $ pure ())

	Crc.resetCrc32 "foobar"
	_ <- doWhile_ block Pipe.=$=
		RunLength.runlength "foobar" Pipe.=$=
		PipeT.convert (either (LBS.pack . (: [])) id) Pipe.=$=
--		PipeT.convert RunLength.toByteString Pipe.=$=
		Crc.crc32 "foobar" Pipe.=$= PipeLBS.length "foobar"
	Crc.compCrc32 "foobar"

	crc <- State.getN "foobar"
	ln <- State.getN "foobar"

	IO.print crc
	IO.print ln

	State.putN "foobar" $ OnDemand.RequestBytes 4
	Just crc0 <- Crc.crc32FromByteString <$> (Except.getRight @String "bad" =<< Pipe.await)
	Just ln0 <- PipeLBS.lengthFromByteString 4 <$> (Except.getRight @String "bad" =<< Pipe.await)

	when (crc /= crc0) $ Except.throw @String "CRC32 error"
	when (ln /= ln0) $ Except.throw @String "length error"

	Pipe.yield ""

block :: (
	U.Member Pipe.P es,
	U.Member (State.S Huffman.Phase) es,
	U.Member (State.S (Huffman.IsLiteral Int)) es,
	U.Member (State.Named "foobar" (Huffman.BinTreePair Int)) es,
	U.Member (State.Named "foobar" Huffman.BitArray) es,
	U.Member (State.Named "foobar" Huffman.ExtraBits) es,
	U.Member (State.Named "foobar" OnDemand.Request) es,
	U.Member (Except.E String) es,
	U.Member Fail.F es
	) =>
	Eff.E es (Either BitArray.B LBS.ByteString) RunLength.R Bool
block = do
	State.putN "foobar" $ OnDemand.RequestBits 1
	Just bf <- either
		(Just . BitArray.toBits @Word8)
		(const Nothing) <$> Pipe.await
	State.putN "foobar" $ OnDemand.RequestBits 2
	Just bt <- either
		(Just . BitArray.toBits @Word8)
		(const Nothing) <$> Pipe.await
	(bf /= 1) <$ case bt of
		0 -> do
			State.putN "foobar" $ OnDemand.RequestBytes 4
			ln <- pairToLength =<< PipeT.skipLeft1
			State.putN "foobar" $ OnDemand.RequestBytes ln
			Pipe.yield . RunLength.LiteralBS =<< Except.getRight @String "bad" =<< Pipe.await
		_	| bt == 1 -> do
			State.putN "foobar" $ OnDemand.RequestBuffer 100
			huffmanBits "foobar"

			State.putN "foobar" . OnDemand.RequestPushBack =<< State.getsN "foobar" Huffman.unBitArray
			State.putN "foobar" $ Huffman.BitArray BitArray.empty
			Right "" <- Pipe.await; pure ()

		_ -> error "yet"
	where
	pairToLength bs0 = fromIntegral @Word16 ln <$ do
		when (LBS.length bs0 /= 4) $ Except.throw @String "not 4 bytes"
		when (ln /= complement cln) $ Except.throw @String "bad pair"
		where (ln, cln) = (LBS.toBits *** LBS.toBits) $ LBS.splitAt 2 bs0

huffmanBits :: forall (nm :: Symbol) -> (
	U.Member Pipe.P es,
	U.Member (State.S Huffman.Phase) es,
	U.Member (State.S (Huffman.IsLiteral Int)) es,
	U.Member (State.Named nm (Huffman.BinTreePair Int)) es,
	U.Member (State.Named nm Huffman.BitArray) es,
	U.Member (State.Named nm Huffman.ExtraBits) es,
	U.Member Fail.F es
	) =>
	Eff.E es (Either BitArray.B LBS.ByteString) RunLength.R ()
huffmanBits nm = void do
	Huffman.huffman' @Int @Word16 nm Pipe.=$= do
		let	(ht, hdt) = (
				Huffman.makeTree [0 :: Int ..] fixedHuffmanList,
				Huffman.makeTree [0 :: Int ..] fixedHuffmanDstList )

		State.put Huffman.PhaseLitLen
		State.put $ Huffman.IsLiteral \i -> (0 :: Int) <= i && i <= 255

		Huffman.putTree nm ht
		litLen' nm ht hdt 0

litLen' :: forall (nm :: Symbol) -> (
	U.Member Pipe.P es,
	U.Member (State.S Huffman.Phase) es,
	U.Member (State.Named nm (Huffman.BinTreePair Int)) es,
	U.Member (State.Named nm Huffman.ExtraBits) es
	) =>
	Huffman.BinTree Int -> Huffman.BinTree Int -> Int ->
	Eff.E es (Either Int Word16) RunLength.R ()
litLen' nm t dt pri = Pipe.await >>= \case
	Left 256 -> pure ()
	Left i	| 0 <= i && i <= 255 -> do
			Pipe.yield (RunLength.Literal $ fromIntegral i)
			litLen' nm t dt 0
		| 257 <= i && i <= 264 -> Huffman.putTree nm dt >> dist' nm t dt (calcLength i 0) 0
		| 265 <= i && i <= 284 -> do
			Huffman.putExtraBits nm $ (fromIntegral i - 261) `div` 4
			litLen' nm t dt i
		| i == 285 -> Huffman.putTree nm dt >> dist' nm t dt (calcLength i 0) 0
	Right eb -> do
		Huffman.putTree nm dt
		dist' nm t dt (calcLength pri eb) 0
	r -> error $ "litLen': " ++ show r

dist' :: forall (nm :: Symbol) -> (
	U.Member Pipe.P es,
	U.Member (State.S Huffman.Phase) es,
	U.Member (State.Named nm (Huffman.BinTreePair Int)) es,
	U.Member (State.Named nm Huffman.ExtraBits) es
	) =>
	Huffman.BinTree Int -> Huffman.BinTree Int -> RunLength.Length -> Int ->
	Eff.E es (Either Int Word16) RunLength.R ()
dist' nm t dt ln pri = Pipe.await >>= \case
	Left i	| 0 <= i && i <= 3 -> do
			Pipe.yield $ RunLength.LenDist ln (calcDist i 0)
			State.put Huffman.PhaseLitLen
			Huffman.putTree nm t
			litLen' nm t dt 0
		| 4 <= i && i <= 29 -> do
			Huffman.putExtraBits nm $ (fromIntegral i - 2) `div` 2
			dist' nm t dt ln i
	Right eb -> do
		Pipe.yield (RunLength.LenDist ln (calcDist pri eb))
		State.put Huffman.PhaseLitLen
		Huffman.putTree nm t
		litLen' nm t dt 0
	r -> error $ "dist': " ++ show r
