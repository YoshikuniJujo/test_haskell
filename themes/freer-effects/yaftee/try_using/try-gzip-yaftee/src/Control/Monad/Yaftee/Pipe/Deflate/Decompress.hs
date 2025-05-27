{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.Deflate.Decompress (

	run_, States,

	decompress, decompress', Members,

	BitArray, FormatBuffer

	) where

import GHC.TypeLits
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
import Data.Gzip.Calc

import Pipe.Huffman qualified as Huffman
import Pipe.Runlength qualified as RunLength

import Data.TypeLevel.List

run_ :: forall (nm :: Symbol) es i o a . HFunctor.Loose (U.U es) =>
	Eff.E  (States nm `Append` es) i o a -> Eff.E es i o ()
run_ = void
	. (`St.runN` FormatBuffer "")
	. (`St.runN` BitArray (BitArray.fromByteString ""))
	. RunLength.run_ . Huffman.run . OnDemand.run_ @nm
	. PipeB.lengthRun . Crc.runCrc32

type States nm = '[ 
	St.Named nm Crc.Crc32,
	St.Named nm PipeB.Length ] `Append`
	OnDemand.States nm `Append`
	Huffman.States nm Int `Append`
	RunLength.States nm `Append` [
	St.Named nm BitArray,
	St.Named nm FormatBuffer ]

decompress :: forall nm -> (
	U.Member Pipe.P es,
	Members nm es,
	U.Member (Except.E String) es, U.Member Fail.F es ) => Int ->
	Eff.E es (Either BitArray.B BS.ByteString) BS.ByteString ()
decompress nm ln =
	void $ doWhile_ (block nm) Pipe.=$= RunLength.runlength nm Pipe.=$= format nm ln Pipe.=$=
		PipeB.length' nm Pipe.=$= Crc.crc32' nm

decompress' :: forall nm -> (
	U.Member Pipe.P es,
	Members nm es,
	U.Member (Except.E String) es, U.Member Fail.F es ) => Int -> Int -> Int ->
	Eff.E es (Either BitArray.B BS.ByteString) BS.ByteString ()
decompress' nm w h bpp =
	void $ doWhile_ (block nm) Pipe.=$= RunLength.runlength nm Pipe.=$= format' nm w h bpp Pipe.=$=
		PipeB.length' nm Pipe.=$= Crc.crc32' nm

type Members nm es = (
	OnDemand.Members nm es,
	Huffman.Members nm Int es,
	RunLength.Members nm es,
	U.Member (St.Named nm Crc.Crc32 ) es,
	U.Member (St.Named nm PipeB.Length) es,
	U.Member (St.Named nm BitArray) es,
	U.Member (St.Named nm FormatBuffer) es )

nvrocc :: String
nvrocc = "Never occur"

block :: forall nm -> (
	U.Member Pipe.P es,
	U.Member (St.Named nm OnDemand.Request) es,
	U.Member (St.Named nm BitArray) es,
	U.Member (St.Named nm (Huffman.BinTreePair Int)) es,
	U.Member (St.Named nm Huffman.ExtraBits) es,
	U.Member (Except.E String) es,
	U.Member (U.FromFirst U.Fail) es ) =>
	Eff.E es (Either BitArray.B BS.ByteString) RunLength.R Bool
block nm = do
	St.putN nm $ OnDemand.RequestBits 1
	Just bf <- either (Just . BitArray.toBits @Word8) (const Nothing) <$> Pipe.await
	St.putN nm $ OnDemand.RequestBits 2
	Just bt <- either (Just . BitArray.toBits @Word8) (const Nothing) <$> Pipe.await
	(bf /= 1) <$ case bt of
		0 -> do	St.putN nm $ OnDemand.RequestBytes 4
			ln <- pairToLength =<< PipeT.skipLeft1
			St.putN nm $ OnDemand.RequestBytes ln
			Pipe.yield . RunLength.LiteralBS =<< Except.getRight @String "bad" =<< Pipe.await
		_	| bt == 1 || bt == 2 -> do
				(mhlithdist, mhclen) <- whenDef (Nothing, Nothing) (bt == 2) do
					St.putN nm $ OnDemand.RequestBits 5
					hlit <- (+ 257) . BitArray.toBits <$> (Except.getLeft @String "bad" =<< Pipe.await)
					hdist <- (+ 1) . BitArray.toBits <$> (Except.getLeft @String "bad" =<< Pipe.await)
					St.putN nm $ OnDemand.RequestBits 4
					hclen <- (+ 4) . BitArray.toBits <$> (Except.getLeft @String "bad" =<< Pipe.await)
					pure (Just (hlit, hlit + hdist), Just hclen)
				St.putN nm $ OnDemand.RequestBuffer 1000
				void $ bits nm Pipe.=$= do

					whenMaybe mhclen \hclen -> do
						rtt <- replicateM hclen (Bit.listToNum @Word8 <$> replicateM 3 Pipe.await)
						let tt = Huffman.makeTree codeLengthList rtt
						Huffman.putTree nm tt

					Huffman.huffman @Int @Word16 nm Pipe.=$= do
						(ht, hdt) <- whenMaybeDef (
								Huffman.makeTree [0 :: Int ..] fixedHuffmanList,
								Huffman.makeTree [0 :: Int ..] fixedHuffmanDstList ) mhlithdist \(hlit, hlitdist) ->
							(Huffman.makeTree [0 :: Int ..] *** Huffman.makeTree [0 :: Int ..]) . splitAt hlit <$> codeLengths nm 0 hlitdist
						Huffman.putTree nm ht
						litLen nm ht hdt 0
				St.putN nm . OnDemand.RequestPushBack =<< St.getsN nm unBitArray
				St.putN nm $ BitArray BitArray.empty
				Right "" <- Pipe.await; pure ()
		_ -> error "bad"
	where
	pairToLength bs0 = fromIntegral @Word16 ln <$ do
		when (BS.length bs0 /= 4) $ Except.throw @String "not 4 bytes"
		when (ln /= complement cln) $ Except.throw @String "bad pair"
		where (ln, cln) = (BS.toBits *** BS.toBits) $ BS.splitAt 2 bs0

codeLengths :: forall nm -> (
	U.Member Pipe.P es,
	U.Member (St.Named nm Huffman.ExtraBits) es,
	U.Member Fail.F es, Integral b ) =>
	Int -> Int -> Eff.E es (Either Int b) o [Int]
codeLengths nm = fix \go pr n -> if n == 0 then pure [] else Pipe.await >>= \case
	Left al	| 0 <= al && al <= 15 -> (al :) <$> go al (n - 1)
		| al `elem` [16, 17, 18] -> do
			let	(ln, eb, k) = fromJust $ lookup al [
					(16, (pr, 2, 3)),
					(17, (0, 3, 3)), (18, (0, 7, 11)) ]
			Huffman.putExtraBits nm eb
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

format' :: forall nm -> (
	U.Member Pipe.P es,
	U.Member (St.Named nm FormatBuffer) es ) =>
	Int -> Int -> Int -> Eff.E es (Either Word8 BS.ByteString) BS.ByteString ()
-- format' nm w h bpp = ($ interlacePixelNums w h) $ fix \go (((+ 1) . (* bpp) -> n) : ns) -> St.getsN nm unFormatBuffer >>= \bs -> bool
format' nm w h bpp = ($ interlacePixelNums w h) $ fix \go -> \case
	[] -> pure ()
	na@(((+ 1) . (* bpp) -> n) : ns) -> St.getsN nm unFormatBuffer >>= \bs -> bool
		(uncurry (>>) ((Pipe.yield *** St.putN nm . FormatBuffer) $ BS.splitAt n bs) >> go ns)
		(maybe (Pipe.yield bs) ((>> go na)
			. St.putN nm . FormatBuffer
			. either (BS.snoc bs) (BS.append bs)) =<< Pipe.awaitMaybe)
		(BS.length bs < n)

interlacePixelNums :: Int -> Int -> [Int]
interlacePixelNums w h =
	replicate (h `div'` 8) (w `div'` 8) ++
	replicate (h `div'` 8) (w `div'` 4 `div` 2) ++
	replicate (h `div'` 4 `div` 2) (w `div'` 4) ++
	replicate (h `div'` 4) (w `div'` 2 `div` 2) ++
	replicate (h `div'` 2 `div` 2) (w `div'` 2) ++
	replicate (h `div'` 2) (w `div` 2) ++
	replicate (h `div` 2) w ++ [0]

m `div'`n = (m - 1) `div` n + 1
