{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Foreign.C.Types
import Control.Arrow
import Control.Monad
import Control.Monad.Fix
import Control.Monad.ToolsYj
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.List qualified as PipeL
import Control.Monad.Yaftee.Pipe.IO qualified as PipeI
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeB
import Control.Monad.Yaftee.Pipe.ByteString.OnDemand qualified as OnDemand
import Control.Monad.Yaftee.Pipe.ByteString.Crc qualified as Crc
import Control.Monad.Yaftee.State qualified as St
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.Fail qualified as Fail
import Control.Monad.Yaftee.IO qualified as IO
import Control.HigherOpenUnion qualified as U
import Data.Bits
import Data.Maybe
import Data.Sequence qualified as Seq
import Data.Bool
import Data.Word
import Data.ByteString qualified as BS
import Data.ByteString.ToolsYj qualified as BS
import Data.ByteString.Bit qualified as Bit
import Data.ByteString.BitArray qualified as BitArray
import Data.Gzip
import Data.Gzip.GzipHeader
import Data.Gzip.Calc
import System.IO
import System.Environment

import Pipe.Huffman qualified as Huffman
import Pipe.RunLength qualified as RunLength

main :: IO ()
main = do
	fp : _ <- getArgs
	h <- openFile fp ReadMode
	let	processHeader = IO.print
	void . Eff.runM . Except.run @String . Fail.runExc id
		. (`St.run` OnDemand.RequestBuffer 16)
		. (`St.run` BitArray.fromByteString "")
		. (`St.run` (Seq.empty :: Seq.Seq Word8))
		. (flip (St.runN @Fmt) ("" :: BS.ByteString))
		. Huffman.run (Huffman.makeTree [0 :: Int .. ] fixedHuffmanList)
		. (flip (St.runN @"bits") $ BitArray.fromByteString "")
		. PipeB.lengthRun . Crc.runCrc32
		. PipeL.to
		$ PipeB.hGet' 64 h Pipe.=$= OnDemand.onDemand Pipe.=$= do
			_ <- PipeT.checkRight Pipe.=$= readHeader processHeader
			doWhile_ block Pipe.=$= RunLength.runLength Pipe.=$= format 32 Pipe.=$=
				PipeB.length' Pipe.=$= Crc.crc32' Pipe.=$= do
					PipeI.print'
					Crc.compCrc32
					IO.print . Crc.crc32ToByteString =<< St.getN PipeB.Pkg
					IO.print . PipeB.lengthToByteString =<< St.getN PipeB.Pkg
					IO.print @BitArray.B =<< St.get
					IO.print @BitArray.B =<< St.getN "bits"

readHeader :: (
	U.Member Pipe.P es,
	U.Member (St.Named PipeB.Pkg Crc.Crc32) es,
	U.Member (St.S OnDemand.Request) es,
	U.Member (Except.E String) es,
	U.Member (U.FromFirst U.Fail) es ) =>
	(GzipHeader -> Eff.E es BS.ByteString o r) ->
	Eff.E es BS.ByteString o (
		Eff.E es BS.ByteString BS.ByteString r1,
		Eff.E es BS.ByteString o r )
readHeader f = Crc.crc32 Pipe.=$= do
				St.put $ OnDemand.RequestBytes 2
				ids <- Pipe.await
				when (ids /= "\31\139")
					$ Except.throw @String "Bad magic"
				St.put $ OnDemand.RequestBytes 1
				cm <- (CompressionMethod . BS.head) <$> Pipe.await
				Just flgs <- readFlags . BS.head <$> Pipe.await
				St.put $ OnDemand.RequestBytes 4
				mtm <- CTime . BS.toBits <$> Pipe.await
				St.put $ OnDemand.RequestBytes 1
				ef <- BS.head <$> Pipe.await
				os <- OS . BS.head <$> Pipe.await
				mexflds <- if (flagsRawExtra flgs)
				then do	St.put $ OnDemand.RequestBytes 2
					xlen <- BS.toBits <$> Pipe.await
					St.put $ OnDemand.RequestBytes xlen
					decodeExtraFields <$> Pipe.await
				else pure []
				St.put OnDemand.RequestString
				mnm <- if flagsRawName flgs
				then Just <$> Pipe.await
				else pure Nothing
				mcmmt <- if flagsRawComment flgs
				then Just <$> Pipe.await
				else pure Nothing
				when (flagsRawHcrc flgs) do
					Crc.compCrc32
					crc <- (.&. 0xffff) . (\(Crc.Crc32 c) -> c) <$> St.getN PipeB.Pkg
					St.put $ OnDemand.RequestBytes 2
					m <- BS.toBits <$> Pipe.await
					when (crc /= m) $
						Except.throw @String "Header CRC check failed"
				f GzipHeader {
					gzipHeaderCompressionMethod = cm,
					gzipHeaderFlags = Flags {
						flagsText = flagsRawText flgs,
						flagsHcrc = flagsRawHcrc flgs },
					gzipHeaderModificationTime = mtm,
					gzipHeaderExtraFlags = ef,
					gzipHeaderOperatingSystem = os,
					gzipHeaderExtraField = mexflds,
					gzipHeaderFileName = mnm,
					gzipHeaderComment = mcmmt }

block :: (
	U.Member Pipe.P es,
	U.Member (St.S OnDemand.Request) es,
	U.Member (St.Named "bits" BitArray.B) es,
	U.Member (St.Named Huffman.Pkg (Huffman.BinTree Int, Huffman.BinTree Int)) es,
	U.Member (St.Named Huffman.Pkg Huffman.ExtraBits) es,
	U.Member (Except.E String) es,
	U.Member (U.FromFirst U.Fail) es ) =>
	Eff.E es (Either BitArray.B BS.ByteString) RunLength.R Bool
block = do
	St.put $ OnDemand.RequestBits 1
	Just bf <- either (Just . BitArray.toBits @Word8) (const Nothing) <$> Pipe.await
	St.put $ OnDemand.RequestBits 2
	Just bt <- either (Just . BitArray.toBits @Word8) (const Nothing) <$> Pipe.await
	(bf /= 1) <$ case bt of
		0 -> do	St.put $ OnDemand.RequestBytes 4
			ln <- pairToLength =<< PipeT.skipLeft1
			St.put $ OnDemand.RequestBytes ln
			Pipe.yield . RunLength.LiteralBS =<< Except.getRight =<< Pipe.await
		_	| bt == 1 || bt == 2 -> do
				(mhlithdist, mhclen) <- whenDef (Nothing, Nothing) (bt == 2) do
					St.put $ OnDemand.RequestBits 5
					hlit <- (+ 257) . BitArray.toBits <$> (Except.getLeft =<< Pipe.await)
					hdist <- (+ 1) . BitArray.toBits <$> (Except.getLeft =<< Pipe.await)
					St.put $ OnDemand.RequestBits 4
					hclen <- (+ 4) . BitArray.toBits <$> (Except.getLeft =<< Pipe.await)
					pure (Just (hlit, hlit + hdist), Just hclen)
				St.put $ OnDemand.RequestBuffer 100
				void $ bits Pipe.=$= do

					whenMaybe mhclen \hclen -> do
						rtt <- replicateM hclen (Bit.listToNum @Word8 <$> replicateM 3 Pipe.await)
						let tt = Huffman.makeTree codeLengthList rtt
						Huffman.putTree tt

					Huffman.huffman @Int @Word16 Pipe.=$= do
						(ht, hdt) <- whenMaybeDef (
								Huffman.makeTree [0 :: Int ..] fixedHuffmanList,
								Huffman.makeTree [0 :: Int ..] fixedHuffmanDstList ) mhlithdist \(hlit, hlitdist) ->
							(Huffman.makeTree [0 :: Int ..] *** Huffman.makeTree [0 :: Int ..]) . splitAt hlit <$> codeLengths 0 hlitdist
						Huffman.putTree ht
						litLen ht hdt 0
				St.put . OnDemand.RequestPushBack =<< St.getN "bits"
				St.putN "bits" BitArray.empty
				Right "" <- Pipe.await; pure ()
		_ -> error "bad"
	where
	pairToLength bs0 = fromIntegral @Word16 ln <$ do
		when (BS.length bs0 /= 4) $ Except.throw @String "not 4 bytes"
		when (ln /= complement cln) $ Except.throw @String "bad pair"
		where (ln, cln) = (BS.toBits *** BS.toBits) $ BS.splitAt 2 bs0

codeLengths :: (
	U.Member Pipe.P es,
	U.Member (St.Named Huffman.Pkg Huffman.ExtraBits) es,
	U.Member Fail.F es, Integral b ) =>
	Int -> Int -> Eff.E es (Either Int b) o [Int]
codeLengths = fix \go pr n -> if n == 0 then pure [] else Pipe.await >>= \case
	Left al	| 0 <= al && al <= 15 -> (al :) <$> go al (n - 1)
		| al `elem` [16, 17, 18] -> do
			let	(ln, eb, k) = fromJust $ lookup al [
					(16, (pr, 2, 3)),
					(17, (0, 3, 3)), (18, (0, 7, 11)) ]
			Huffman.putExtraBits eb
			Right ((+ k) . fromIntegral -> rp) <- Pipe.await
			(replicate rp ln ++) <$> go pr (n - rp)
		| otherwise -> error "bad"
	Right _ -> error "bad"

litLen :: (
	U.Member Pipe.P es,
	U.Member (St.Named Huffman.Pkg
		(Huffman.BinTree Int, Huffman.BinTree Int)) es,
	U.Member (St.Named Huffman.Pkg Huffman.ExtraBits) es ) =>
	Huffman.BinTree Int -> Huffman.BinTree Int -> Int ->
	Eff.E es (Either Int Word16) RunLength.R ()
litLen t dt pri = Pipe.await >>= \case
	Left 256 -> pure ()
	Left i	| 0 <= i && i <= 255 -> do
			Pipe.yield (RunLength.Literal $ fromIntegral i)
			litLen t dt 0
		| 257 <= i && i <= 264 -> Huffman.putTree dt >> dist t dt (calcLength i 0) 0
		| 265 <= i && i <= 284 -> do
			Huffman.putExtraBits $ (i - 261) `div` 4
			litLen t dt i
		| i == 285 -> Huffman.putTree dt >> dist t dt (calcLength i 0) 0
	Right eb -> do
		Huffman.putTree dt
		dist t dt (calcLength pri eb) 0
	err -> error $ show err

dist :: (
	U.Member Pipe.P es,
	U.Member (St.Named Huffman.Pkg
		(Huffman.BinTree Int, Huffman.BinTree Int)) es,
	U.Member (St.Named Huffman.Pkg Huffman.ExtraBits) es ) =>
	Huffman.BinTree Int -> Huffman.BinTree Int -> RunLength.Length -> Int ->
	Eff.E es (Either Int Word16) RunLength.R ()
dist t dt ln pri = Pipe.await >>= \case
	Left i	| 0 <= i && i <= 3 -> do
			Pipe.yield $ RunLength.LenDist ln (calcDist i 0)
			Huffman.putTree t
			litLen t dt 0
		| 4 <= i && i <= 29 -> do
			Huffman.putExtraBits $ (i - 2) `div` 2
			dist t dt ln i
		| otherwise -> error $ "putDist: yet " ++ show i
	Right eb -> do
		Pipe.yield (RunLength.LenDist ln (calcDist pri eb))
		Huffman.putTree t
		litLen t dt 0

bits :: (U.Member Pipe.P es, U.Member (St.Named "bits" BitArray.B) es) =>
	Eff.E es (Either BitArray.B BS.ByteString) Bit.B r
bits = forever . (Pipe.yield =<<)
	$ fix \go -> St.getsN "bits" BitArray.pop >>= maybe
		((>> go) $ St.putN "bits"
			. either id BitArray.fromByteString =<< Pipe.await)
		(uncurry (<$) . (St.putN "bits" `second`))

format :: (
	U.Member Pipe.P es,
	U.Member (St.Named Fmt BS.ByteString) es ) =>
	Int -> Eff.E es (Either Word8 BS.ByteString) BS.ByteString ()
format n = fix \go -> St.getN Fmt >>= \bs -> bool
	(uncurry (>>) ((Pipe.yield *** St.putN Fmt) $ BS.splitAt n bs) >> go)
	(maybe (Pipe.yield bs) ((>> go)
		. St.putN Fmt
		. either (BS.snoc bs) (BS.append bs)) =<< Pipe.awaitMaybe)
	(BS.length bs < n)

type Fmt = "format"
