{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.Gzip.Decompress (

	run_, States,

	decompress, Members,

	BitArray, FormatBuffer

	) where

import Foreign.C.Types
import Control.Arrow
import Control.Monad
import Control.Monad.Fix
import Control.Monad.ToolsYj
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeB
import Control.Monad.Yaftee.Pipe.ByteString.OnDemand qualified as OnDemand
import Control.Monad.Yaftee.Pipe.ByteString.Crc qualified as Crc
import Control.Monad.Yaftee.State qualified as St
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.Fail qualified as Fail
import Control.HigherOpenUnion qualified as U
import Data.HigherFunctor qualified as HFunctor
import Data.Bits
import Data.Maybe
import Data.Bool
import Data.Word
import Data.ByteString qualified as BS
import Data.ByteString.ToolsYj qualified as BS
import Data.ByteString.Bit qualified as Bit
import Data.ByteString.BitArray qualified as BitArray
import Data.Gzip
import Data.Gzip.GzipHeader
import Data.Gzip.Calc

import Pipe.Huffman qualified as Huffman
import Pipe.Runlength qualified as RunLength

import Data.TypeLevel.List

run_ :: HFunctor.Loose (U.U es) =>
	Eff.E  (States "foobar" `Append` es) i o a -> Eff.E es i o ()
run_ = void
	. (flip (St.runN @"foobar") $ FormatBuffer "")
	. (flip (St.runN @"foobar") . BitArray $ BitArray.fromByteString "")
	. PipeB.lengthRun @"foobar" . Crc.runCrc32 @"foobar"
	. RunLength.run_ . Huffman.run . OnDemand.run_

type States nm =
	OnDemand.States nm `Append`
	Huffman.States nm Int `Append`
	RunLength.States nm `Append` [
	St.Named nm Crc.Crc32,
	St.Named nm PipeB.Length,
	St.Named nm BitArray,
	St.Named nm FormatBuffer ]

decompress :: (
	U.Member Pipe.P es,
	Members "foobar" es,
	U.Member (Except.E String) es, U.Member Fail.F es ) =>
	(GzipHeader -> Eff.E es BS.ByteString BS.ByteString r) ->
	Eff.E es BS.ByteString BS.ByteString ()
decompress phd = void $ OnDemand.onDemand "foobar" Pipe.=$= do
	_ <- PipeT.checkRight Pipe.=$= readHeader phd
	_ <- doWhile_ block Pipe.=$= RunLength.runlength "foobar" Pipe.=$= format "foobar" 32 Pipe.=$=
		PipeB.length' "foobar" Pipe.=$= Crc.crc32' "foobar"
	Crc.compCrc32 "foobar"

	crc <- St.getN "foobar"
	St.putN "foobar" $ OnDemand.RequestBytes 4
	crc' <- Except.fromJust nvrocc . Crc.byteStringToCrc32 =<< PipeT.skipLeft1
	when (crc /= crc') $ Except.throw @String "bad CRC32"

	ln <- St.getN "foobar"
	ln' <- Except.fromJust nvrocc . PipeB.byteStringToLength
		=<< Except.getRight @String "bad" =<< Pipe.await
	when (ln /= ln') $ Except.throw @String "bad length"

type Members nm es = (
	OnDemand.Members nm es,
	Huffman.Members nm Int es,
	RunLength.Members nm es,
	U.Member (St.Named nm Crc.Crc32 ) es,
	U.Member (St.Named nm PipeB.Length) es,
	U.Member (St.Named nm BitArray) es,
	U.Member (St.Named nm FormatBuffer) es )

readHeader :: (
	U.Member Pipe.P es,
	U.Member (St.Named "foobar" Crc.Crc32) es,
	U.Member (St.Named "foobar" OnDemand.Request) es,
	U.Member (Except.E String) es,
	U.Member (U.FromFirst U.Fail) es ) =>
	(GzipHeader -> Eff.E es BS.ByteString o r) ->
	Eff.E es BS.ByteString o (
		Eff.E es BS.ByteString BS.ByteString r1,
		Eff.E es BS.ByteString o r )
readHeader f = Crc.crc32 "foobar" Pipe.=$= do
				St.putN "foobar" $ OnDemand.RequestBytes 2
				ids <- Pipe.await
				when (ids /= "\31\139")
					$ Except.throw @String "Bad magic"
				St.putN "foobar" $ OnDemand.RequestBytes 1
				cm <- (CompressionMethod . BS.head) <$> Pipe.await
				Just flgs <- readFlags . BS.head <$> Pipe.await
				St.putN "foobar" $ OnDemand.RequestBytes 4
				mtm <- CTime . BS.toBits <$> Pipe.await
				St.putN "foobar" $ OnDemand.RequestBytes 1
				ef <- BS.head <$> Pipe.await
				os <- OS . BS.head <$> Pipe.await
				mexflds <- if (flagsRawExtra flgs)
				then do	St.putN "foobar" $ OnDemand.RequestBytes 2
					xlen <- BS.toBits <$> Pipe.await
					St.putN "foobar" $ OnDemand.RequestBytes xlen
					decodeExtraFields <$> Pipe.await
				else pure []
				St.putN "foobar" OnDemand.RequestString
				mnm <- if flagsRawName flgs
				then Just <$> Pipe.await
				else pure Nothing
				mcmmt <- if flagsRawComment flgs
				then Just <$> Pipe.await
				else pure Nothing
				when (flagsRawHcrc flgs) do
					Crc.compCrc32 "foobar"
					crc <- (.&. 0xffff) . (\(Crc.Crc32 c) -> c) <$> St.getN "foobar"
					St.putN "foobar" $ OnDemand.RequestBytes 2
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

nvrocc :: String
nvrocc = "Never occur"

block :: (
	U.Member Pipe.P es,
	U.Member (St.Named "foobar" OnDemand.Request) es,
	U.Member (St.Named "foobar" BitArray) es,
	U.Member (St.Named "foobar" (Huffman.BinTreePair Int)) es,
	U.Member (St.Named "foobar" Huffman.ExtraBits) es,
	U.Member (Except.E String) es,
	U.Member (U.FromFirst U.Fail) es ) =>
	Eff.E es (Either BitArray.B BS.ByteString) RunLength.R Bool
block = do
	St.putN "foobar" $ OnDemand.RequestBits 1
	Just bf <- either (Just . BitArray.toBits @Word8) (const Nothing) <$> Pipe.await
	St.putN "foobar" $ OnDemand.RequestBits 2
	Just bt <- either (Just . BitArray.toBits @Word8) (const Nothing) <$> Pipe.await
	(bf /= 1) <$ case bt of
		0 -> do	St.putN "foobar" $ OnDemand.RequestBytes 4
			ln <- pairToLength =<< PipeT.skipLeft1
			St.putN "foobar" $ OnDemand.RequestBytes ln
			Pipe.yield . RunLength.LiteralBS =<< Except.getRight @String "bad" =<< Pipe.await
		_	| bt == 1 || bt == 2 -> do
				(mhlithdist, mhclen) <- whenDef (Nothing, Nothing) (bt == 2) do
					St.putN "foobar" $ OnDemand.RequestBits 5
					hlit <- (+ 257) . BitArray.toBits <$> (Except.getLeft @String "bad" =<< Pipe.await)
					hdist <- (+ 1) . BitArray.toBits <$> (Except.getLeft @String "bad" =<< Pipe.await)
					St.putN "foobar" $ OnDemand.RequestBits 4
					hclen <- (+ 4) . BitArray.toBits <$> (Except.getLeft @String "bad" =<< Pipe.await)
					pure (Just (hlit, hlit + hdist), Just hclen)
				St.putN "foobar" $ OnDemand.RequestBuffer 100
				void $ bits "foobar" Pipe.=$= do

					whenMaybe mhclen \hclen -> do
						rtt <- replicateM hclen (Bit.listToNum @Word8 <$> replicateM 3 Pipe.await)
						let tt = Huffman.makeTree codeLengthList rtt
						Huffman.putTree "foobar" tt

					Huffman.huffman @Int @Word16 "foobar" Pipe.=$= do
						(ht, hdt) <- whenMaybeDef (
								Huffman.makeTree [0 :: Int ..] fixedHuffmanList,
								Huffman.makeTree [0 :: Int ..] fixedHuffmanDstList ) mhlithdist \(hlit, hlitdist) ->
							(Huffman.makeTree [0 :: Int ..] *** Huffman.makeTree [0 :: Int ..]) . splitAt hlit <$> codeLengths 0 hlitdist
						Huffman.putTree "foobar" ht
						litLen "foobar" ht hdt 0
				St.putN "foobar" . OnDemand.RequestPushBack =<< St.getsN "foobar" unBitArray
				St.putN "foobar" $ BitArray BitArray.empty
				Right "" <- Pipe.await; pure ()
		_ -> error "bad"
	where
	pairToLength bs0 = fromIntegral @Word16 ln <$ do
		when (BS.length bs0 /= 4) $ Except.throw @String "not 4 bytes"
		when (ln /= complement cln) $ Except.throw @String "bad pair"
		where (ln, cln) = (BS.toBits *** BS.toBits) $ BS.splitAt 2 bs0

codeLengths :: (
	U.Member Pipe.P es,
	U.Member (St.Named "foobar" Huffman.ExtraBits) es,
	U.Member Fail.F es, Integral b ) =>
	Int -> Int -> Eff.E es (Either Int b) o [Int]
codeLengths = fix \go pr n -> if n == 0 then pure [] else Pipe.await >>= \case
	Left al	| 0 <= al && al <= 15 -> (al :) <$> go al (n - 1)
		| al `elem` [16, 17, 18] -> do
			let	(ln, eb, k) = fromJust $ lookup al [
					(16, (pr, 2, 3)),
					(17, (0, 3, 3)), (18, (0, 7, 11)) ]
			Huffman.putExtraBits "foobar" eb
			Right ((+ k) . fromIntegral -> rp) <- Pipe.await
			(replicate rp ln ++) <$> go pr (n - rp)
		| otherwise -> error "bad"
	Right _ -> error "bad"

litLen :: forall nm -> (
	U.Member Pipe.P es,
	U.Member (St.Named nm
		(Huffman.BinTreePair Int)) es,
	U.Member (St.Named nm Huffman.ExtraBits) es ) =>
	Huffman.BinTree Int -> Huffman.BinTree Int -> Int ->
	Eff.E es (Either Int Word16) RunLength.R ()
litLen nm t dt pri = Pipe.await >>= \case
	Left 256 -> pure ()
	Left i	| 0 <= i && i <= 255 -> do
			Pipe.yield (RunLength.Literal $ fromIntegral i)
			litLen nm t dt 0
		| 257 <= i && i <= 264 -> Huffman.putTree nm dt >> dist nm t dt (calcLength i 0) 0
		| 265 <= i && i <= 284 -> do
			Huffman.putExtraBits nm $ (i - 261) `div` 4
			litLen nm t dt i
		| i == 285 -> Huffman.putTree nm dt >> dist nm t dt (calcLength i 0) 0
	Right eb -> do
		Huffman.putTree nm dt
		dist nm t dt (calcLength pri eb) 0
	err -> error $ show err

dist :: forall nm -> (
	U.Member Pipe.P es,
	U.Member (St.Named nm (Huffman.BinTreePair Int)) es,
	U.Member (St.Named nm Huffman.ExtraBits) es ) =>
	Huffman.BinTree Int -> Huffman.BinTree Int -> RunLength.Length -> Int ->
	Eff.E es (Either Int Word16) RunLength.R ()
dist nm t dt ln pri = Pipe.await >>= \case
	Left i	| 0 <= i && i <= 3 -> do
			Pipe.yield $ RunLength.LenDist ln (calcDist i 0)
			Huffman.putTree nm t
			litLen nm t dt 0
		| 4 <= i && i <= 29 -> do
			Huffman.putExtraBits nm $ (i - 2) `div` 2
			dist nm t dt ln i
		| otherwise -> error $ "putDist: yet " ++ show i
	Right eb -> do
		Pipe.yield (RunLength.LenDist ln (calcDist pri eb))
		Huffman.putTree nm t
		litLen nm t dt 0

bits :: forall nm -> (U.Member Pipe.P es, U.Member (St.Named nm BitArray) es) =>
	Eff.E es (Either BitArray.B BS.ByteString) Bit.B r
bits nm = forever . (Pipe.yield =<<)
	$ fix \go -> St.getsN nm (BitArray.pop . unBitArray) >>= maybe
		((>> go) $ St.putN nm . BitArray
			. either id BitArray.fromByteString =<< Pipe.await)
		(uncurry (<$) . ((St.putN nm . BitArray) `second`))

newtype BitArray = BitArray { unBitArray :: BitArray.B } deriving Show

format :: forall nm -> (
	U.Member Pipe.P es,
	U.Member (St.Named nm FormatBuffer) es ) =>
	Int -> Eff.E es (Either Word8 BS.ByteString) BS.ByteString ()
format nm n = fix \go -> St.getsN nm unFormatBuffer >>= \bs -> bool
	(uncurry (>>) ((Pipe.yield *** St.putN nm . FormatBuffer) $ BS.splitAt n bs) >> go)
	(maybe (Pipe.yield bs) ((>> go)
		. St.putN nm . FormatBuffer
		. either (BS.snoc bs) (BS.append bs)) =<< Pipe.awaitMaybe)
	(BS.length bs < n)

newtype FormatBuffer =
	FormatBuffer { unFormatBuffer :: BS.ByteString } deriving Show
