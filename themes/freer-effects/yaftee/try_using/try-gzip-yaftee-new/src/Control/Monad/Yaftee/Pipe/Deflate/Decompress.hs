{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.Deflate.Decompress (
	decompress, Members
	) where

import GHC.TypeLits
import Control.Arrow
import Control.Monad
import Control.Monad.Fix
import Control.Monad.ToolsYj
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.ByteString.Lazy.OnDemand qualified as OnDemand
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.Fail qualified as Fail
import Control.HigherOpenUnion qualified as U
import Data.Bits
import Data.Maybe
import Data.Word
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.ToolsYj qualified as LBS
import Data.ByteString.Lazy.BitArray qualified as BitArray

import Pipe.Huffman qualified as Huffman
import Pipe.Runlength qualified as RunLength

import Data.Gzip
import Data.Gzip.Calc

decompress :: forall (nm :: Symbol) -> (
	U.Member Pipe.P es, Members nm es,
	U.Member (Except.E String) es, U.Member Fail.F es ) =>
	Eff.E es
		(Either BitArray.B LBS.ByteString)
		(Either Word8 LBS.ByteString) ()
decompress nm = void $ doWhile_ (block nm) Pipe.=$= RunLength.runlength nm

type Members nm es = (
	RunLength.Members nm es,
	Huffman.Members nm Int es,
	U.Member (State.Named nm OnDemand.Request) es )

block :: forall nm -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm Huffman.Phase) es,
	U.Member (State.Named nm (Huffman.IsLiteral Int)) es,
	U.Member (State.Named nm (Huffman.BinTreePair Int)) es,
	U.Member (State.Named nm Huffman.BitArray) es,
	U.Member (State.Named nm Huffman.ExtraBits) es,
	U.Member (State.Named nm OnDemand.Request) es,
	U.Member (Except.E String) es,
	U.Member Fail.F es
	) =>
	Eff.E es (Either BitArray.B LBS.ByteString) RunLength.R Bool
block nm = do
	State.putN nm $ OnDemand.RequestBits 1
	Just bf <- either
		(Just . BitArray.toBits @Word8)
		(const Nothing) <$> Pipe.await
	State.putN nm $ OnDemand.RequestBits 2
	Just bt <- either
		(Just . BitArray.toBits @Word8)
		(const Nothing) <$> Pipe.await
	(bf /= 1) <$ case bt of
		0 -> do
			State.putN nm $ OnDemand.RequestBytes 4
			ln <- pairToLength =<< PipeT.skipLeft1
			State.putN nm $ OnDemand.RequestBytes ln
			Pipe.yield . RunLength.LiteralBS =<< Except.getRight @String "bad 3" =<< Pipe.await
		_	| bt == 1 || bt == 2 -> do
			(mhlithdist :: Maybe (Int, Int), mhclen :: Maybe Int) <- whenDef (Nothing, Nothing) (bt == 2) do
				State.putN nm $ OnDemand.RequestBits 5
				hlit <- (+ 257) . BitArray.toBits <$> (Except.getLeft @String "bad 4" =<< Pipe.await)
				hdist <- (+ 1) . BitArray.toBits <$> (Except.getLeft @String "bad 5" =<< Pipe.await)
				State.putN nm $ OnDemand.RequestBits 4
				hclen <- (+ 4) . BitArray.toBits <$> (Except.getLeft @String "bad 6" =<< Pipe.await)
				pure (Just (hlit, hlit + hdist), Just hclen)
			State.putN nm $ OnDemand.RequestBuffer 100
			huffmanBits nm mhclen mhlithdist

			State.putN nm . OnDemand.RequestPushBack =<< State.getsN nm Huffman.unBitArray
			State.putN nm $ Huffman.BitArray BitArray.empty
			Right "" <- Pipe.await; pure ()

		_ -> error "yet"
	where
	pairToLength bs0 = fromIntegral @Word16 ln <$ do
		when (LBS.length bs0 /= 4) $ Except.throw @String "not 4 bytes"
		when (ln /= complement cln) $ Except.throw @String "bad pair"
		where (ln, cln) = (LBS.toBits *** LBS.toBits) $ LBS.splitAt 2 bs0

huffmanBits :: forall (nm :: Symbol) -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm OnDemand.Request) es,
	U.Member (State.Named nm Huffman.Phase) es,
	U.Member (State.Named nm (Huffman.IsLiteral Int)) es,
	U.Member (State.Named nm (Huffman.BinTreePair Int)) es,
	U.Member (State.Named nm Huffman.BitArray) es,
	U.Member (State.Named nm Huffman.ExtraBits) es,
	U.Member Fail.F es
	) =>
	Maybe Int -> Maybe (Int, Int) ->
	Eff.E es (Either BitArray.B LBS.ByteString) RunLength.R ()
huffmanBits nm mhclen mhlithdist = void do

	whenMaybe mhclen \hclen -> do
		State.putN nm $ OnDemand.RequestBits 3
		rtt <- replicateM hclen (BitArray.toBits @Word8 . either id BitArray.fromByteString <$> Pipe.await)
		let tt = Huffman.makeTree codeLengthList rtt
		Huffman.putTree nm tt

	State.putN nm $ OnDemand.RequestBuffer 100

	Huffman.huffman' @Int @Word16 nm Pipe.=$= do
		(ht, hdt) <- whenMaybeDef (
				Huffman.makeTree [0 :: Int ..] fixedHuffmanList,
				Huffman.makeTree [0 :: Int ..] fixedHuffmanDstList ) mhlithdist \(hlit, hlitdist) ->
			(Huffman.makeTree [0 :: Int ..] *** Huffman.makeTree [0 :: Int ..]) . splitAt hlit <$> codeLengths nm 0 hlitdist

		State.putN nm Huffman.PhaseLitLen
		State.putN nm $ Huffman.IsLiteral \i -> (0 :: Int) <= i && i <= 255

		Huffman.putTree nm ht
		litLen' nm ht hdt 0

codeLengths :: forall (nm :: Symbol) -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm Huffman.ExtraBits) es,
	U.Member Fail.F es,
	Integral b
	) =>
	Int -> Int ->
	Eff.E es (Either Int b) o [Int]
codeLengths nm = fix \go pr n -> if n == 0 then pure [] else Pipe.await >>= \case
	Left al	| 0 <= al && al <= 15 -> (al :) <$> go al (n - 1)
		| al `elem` [16, 17, 18] -> do
			let	(ln, eb, k) = fromJust $ lookup al [
					(16, (pr, 2, 3)),
					(17, (0, 3, 3)), (18, (0, 7, 11)) ]
			Huffman.putExtraBits nm eb
			Right ((+ k) . fromIntegral -> rp) <- Pipe.await
			(replicate rp ln ++) <$> go pr (n - rp)
		| otherwise -> error "bad 7"
	Right _ -> error "bad 8"

litLen' :: forall (nm :: Symbol) -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm Huffman.Phase) es,
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
	U.Member (State.Named nm Huffman.Phase) es,
	U.Member (State.Named nm (Huffman.BinTreePair Int)) es,
	U.Member (State.Named nm Huffman.ExtraBits) es
	) =>
	Huffman.BinTree Int -> Huffman.BinTree Int -> RunLength.Length -> Int ->
	Eff.E es (Either Int Word16) RunLength.R ()
dist' nm t dt ln pri = Pipe.await >>= \case
	Left i	| 0 <= i && i <= 3 -> do
			Pipe.yield $ RunLength.LenDist ln (calcDist i 0)
			State.putN nm Huffman.PhaseLitLen
			Huffman.putTree nm t
			litLen' nm t dt 0
		| 4 <= i && i <= 29 -> do
			Huffman.putExtraBits nm $ (fromIntegral i - 2) `div` 2
			dist' nm t dt ln i
	Right eb -> do
		Pipe.yield (RunLength.LenDist ln (calcDist pri eb))
		State.putN nm Huffman.PhaseLitLen
		Huffman.putTree nm t
		litLen' nm t dt 0
	r -> error $ "dist': " ++ show r
