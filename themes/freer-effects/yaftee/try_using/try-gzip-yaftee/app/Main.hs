{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
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
import Data.Sequence qualified as Seq
import Data.Bool
import Data.Word
import Data.ByteString qualified as BS
import Data.ByteString.ToolsYj qualified as BS
import Data.ByteString.Bit qualified as Bit
import Data.ByteString.BitArray qualified as BitArray
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
		. (flip (St.runN @"format") ("" :: BS.ByteString))
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
			ln <- getWord16FromPair =<< PipeT.skipLeft1
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
							(Huffman.makeTree [0 :: Int ..] *** Huffman.makeTree [0 :: Int ..]) . splitAt hlit <$> getCodeTable 0 hlitdist
						Huffman.putTree ht
						litLen ht hdt 0
				St.put . OnDemand.RequestPushBack =<< St.getN "bits"
				St.putN "bits" BitArray.empty
				Right "" <- Pipe.await; pure ()
		_ -> error "bad"

getWord16FromPair :: (U.Member (Except.E String) es, Num n) =>
	BS.ByteString -> Eff.E es i o n
getWord16FromPair bs0 = fromIntegral @Word16 <$> do
	when (BS.length bs0 /= 4)
		$ Except.throw @String "getWord16FromPair: not 4 bytes"
	when (ln /= complement cln)
		$ Except.throw @String "bad pair"
	pure ln
	where (ln, cln) = (BS.toBits *** BS.toBits) $ BS.splitAt 2 bs0

codeLengthList :: [Int]
codeLengthList =
	[16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15]

fixedHuffmanList, fixedHuffmanDstList :: [Int]
fixedHuffmanList =
	replicate 144 8 ++ replicate 112 9 ++ replicate 24 7 ++ replicate 8 8

fixedHuffmanDstList = replicate 32 5

getCodeTable :: (
	U.Member Pipe.P es,
	U.Member (St.Named Huffman.Pkg Huffman.ExtraBits) es,
	U.Member Fail.F es,
	Integral b
	) => Int -> Int -> Eff.E es (Either Int b) o [Int]
getCodeTable _ 0 = pure []
getCodeTable pr n = Pipe.await >>= \case
	Left ln	| 0 <= ln && ln <= 15 -> (ln :) <$> getCodeTable ln (n - 1)
		| ln == 16 -> do
			Huffman.putExtraBits 2
			Right eb <- Pipe.await
			(replicate (fromIntegral eb + 3) pr ++) <$> getCodeTable pr (n - fromIntegral eb - 3)
		| ln == 17 -> do
			Huffman.putExtraBits 3
			Right eb <- Pipe.await
			(replicate (fromIntegral eb + 3) 0 ++) <$> getCodeTable 0 (n - fromIntegral eb - 3)
		| ln == 18 -> do
			Huffman.putExtraBits 7
			Right eb <- Pipe.await
			(replicate (fromIntegral eb + 11) 0 ++) <$> getCodeTable 0 (n - fromIntegral eb - 11)
		| otherwise -> error "yet: "
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
	U.Member (St.Named "format" BS.ByteString) es ) =>
	Int -> Eff.E es (Either Word8 BS.ByteString) BS.ByteString ()
format n = fix \go -> St.getsN "format" ((>= n) . BS.length) >>= bool
	(readMore >>= bool (Pipe.yield =<< St.getN "format") go)
	((>>go) $ uncurry (>>) . (Pipe.yield *** St.putN "format")
		. BS.splitAt n =<< St.getN "format")
	where readMore = (Pipe.isMore >>=) . bool (pure False) . (True <$)
		$ St.modifyN "format" . either (flip BS.snoc) (flip BS.append)
			=<< Pipe.await
