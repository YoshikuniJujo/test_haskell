{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Yaftee.UseFTCQ.Pipe.Gzip.Block (

	blocks, format,


	putDecoded

	) where

import Control.Arrow
import Control.Monad
import Control.Monad.Fix
import Yaftee.UseFTCQ.Eff qualified as Eff
import Yaftee.UseFTCQ.Pipe qualified as Pipe
import Yaftee.UseFTCQ.State qualified as State
import Yaftee.UseFTCQ.Except qualified as Except
import Yaftee.UseFTCQ.Fail qualified as Fail
import Yaftee.OpenUnion qualified as Union
import Data.Foldable
import Data.Bits
import Data.Sequence qualified as Seq
import Data.Bool
import Data.Word
import Data.ByteString qualified as BS

import Data.Bit qualified as Bit
import Data.BitArrayNew qualified as BitArray
import Data.HuffmanTree (BinTree, mkTr, fixedTable, fixedDstTable)
import Yaftee.UseFTCQ.Pipe.Gzip.Huffman
import Data.Calc
import Yaftee.UseFTCQ.Pipe.Gzip.RunLength qualified as RunLength

import Yaftee.UseFTCQ.Pipe.ByteString.OnDemand

import Yaftee.UseFTCQ.Pipe.BitArray

import Debug.Trace

blocks :: (
	Union.Member Pipe.P effs,
	Union.Member (State.S Request) effs,
	Union.Member (State.S (BinTree Int, BinTree Int)) effs,
	Union.Member (State.S ExtraBits) effs,
	Union.Member (State.Named "bits" BitArray.B) effs,
	Union.Member (State.S (Seq.Seq Word8)) effs,
	Union.Member (Except.E String) effs,
	Union.Member Fail.F effs ) =>
	Eff.E effs
		(Either BitArray.B BS.ByteString)
		(Either Word8 BS.ByteString) ()
blocks = fix (\go -> block' >>= bool (pure ()) go) Pipe.=$= RunLength.runLength >> pure ()

block' :: (
	Union.Member Pipe.P effs,
	Union.Member (State.S Request) effs,
	Union.Member (Except.E String) effs,
	Union.Member Fail.F effs,
	Union.Member (State.S (BinTree Int, BinTree Int)) effs,
	Union.Member (State.S ExtraBits) effs,
	Union.Member (State.Named "bits" BitArray.B) effs ) =>
	Eff.E effs (Either BitArray.B BS.ByteString) RunLength.R Bool
block' = do
	trace "here" (pure ())
	State.put $ RequestBits 1
	trace "there" (pure ())
	Left (Just t) <- (either (Left . BitArray.toBits') Right)
		<$> Pipe.await
	trace "here 2" (pure ())
	State.put $ RequestBits 2
	Just bt <- BitArray.toBits' <$> (getLeft =<< Pipe.await)
	trace (show t) (pure ())
	trace (show bt) (pure ())
	case bt of
		(0 :: Word8) -> do
			State.put $ RequestBytes 4
			ln <- getWord16FromPair =<< skipLeft1
			for_ (separate 10 ln) \ln' -> do
				State.put $ RequestBytes ln'
				Pipe.yield . RunLength.LiteralBS =<< getRight =<< Pipe.await
		_	| bt == 1 || bt == 2 -> do
			(mhlithdist, mhclen) <- whenDef (Nothing, Nothing) (bt == 2) do
				State.put $ RequestBits 5
				Just hlit <- ((+ 257) <$>)
					. BitArray.toBits' <$> (getLeft =<< Pipe.await)
				Just hdist <- ((+ 1) <$>)
					. BitArray.toBits' <$> (getLeft =<< Pipe.await)
				State.put $ RequestBits 4
				Just (hclen :: Int) <- ((+ 4) <$>)
					. BitArray.toBits' <$> (getLeft =<< Pipe.await)
				trace ("(hlit, hdist, hclen) = " ++ show (hlit, hdist, hclen)) $ pure ()
				pure (Just (hlit, hlit + hdist), Just hclen)
			_ <- bits Pipe.=$= bitsBlock mhclen mhlithdist
			bf <- State.getN "bits"
			trace (show bf) (pure ())
			(dbg :: Request) <- State.get
			trace (show dbg) $ pure ()
			State.putN "bits" BitArray.empty
			State.put $ RequestPushBack bf
			Right "" <- Pipe.await
			pure ()
		_ -> Except.throw $ "No such BType: " ++ show bt
	pure (t /= (1 :: Word8))

bitsBlock :: (
	Union.Member Pipe.P effs,
	Union.Member (State.S (BinTree Int, BinTree Int)) effs,
	Union.Member (State.S ExtraBits) effs,
	Union.Member Fail.F effs ) =>
	Maybe Int -> Maybe (Int, Int) ->
	Eff.E effs Bit.B RunLength.R ()
bitsBlock mhclen mhlithdist = do
	whenMaybe mhclen \hclen -> (\x -> State.put . (\y -> trace (show y) $ (id &&& id) y) =<< x)
		. (mkTr @Word8 codeLengthList <$>)
		. replicateM hclen . (Bit.listToNum <$>)
		$ replicateM 3 Pipe.await
	_ <- huffmanPipe Pipe.=$= do
		(lct, dct) <- whenMaybeDef
			(fixedTable, fixedDstTable) mhlithdist \(hlit, hld) ->
			(mkTr [0 ..] *** mkTr [0 ..])
			. (\x -> trace (show (length x) ++ " " ++ show x) $ Prelude.splitAt hlit x) <$> getCodeTable 0 hld
		trace ("(lct, dct) = " ++ show (lct, dct)) $ pure ()
		State.put $ (id &&& id) lct
		putDecoded lct dct 0
	pure ()

whenDef :: Applicative m => a -> Bool -> m a -> m a
whenDef d b a = bool (pure d) a b

whenMaybe :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenMaybe mx f = case mx of Nothing -> pure (); Just x -> f x

whenMaybeDef :: Applicative m => b -> Maybe a -> (a -> m b) -> m b
whenMaybeDef d mx f = case mx of Nothing -> pure d; Just x -> f x

codeLengthList :: [Int]
codeLengthList =
	[16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15]

putDecoded :: (
	Union.Member Pipe.P effs,
	Union.Member (State.S (BinTree Int, BinTree Int)) effs,
	Union.Member (State.S ExtraBits) effs ) =>
	BinTree Int -> BinTree Int -> Int ->
	Eff.E effs (Either Int Word16) RunLength.R ()
putDecoded t dt pri = do
	mi <- Pipe.await
	case mi of
		Left 256 -> pure ()
		Left i
			| 0 <= i && i <= 255 -> do
				Pipe.yield (RunLength.Literal $ fromIntegral i)
				putDecoded t dt 0
			| 257 <= i && i <= 264 -> State.put (dt, dt) >> putDist t dt (calcLength i 0) 0
			| 265 <= i && i <= 284 -> do
				State.put . ExtraBits $ (i - 261) `div` 4
				putDecoded t dt i
			| i == 285 -> State.put (dt, dt) >> putDist t dt (calcLength i 0) 0
			| otherwise -> error $ "putDecoded: yet " ++ show i
		Right eb -> do
			State.put (dt, dt)
			putDist t dt (calcLength pri eb) 0

putDist :: (
	Union.Member Pipe.P effs,
	Union.Member (State.S (BinTree Int, BinTree Int)) effs,
	Union.Member (State.S ExtraBits) effs
	) =>
	BinTree Int -> BinTree Int -> RunLength.Length -> Int ->
	Eff.E effs (Either Int Word16) RunLength.R ()
putDist t dt ln pri = do
	mi <- Pipe.await
--	Pipe.print' mi
	case mi of
		Left i
			| 0 <= i && i <= 3 -> do
				Pipe.yield (RunLength.LenDist ln (calcDist i 0))
				State.put (t, t)
				putDecoded t dt 0
			| 4 <= i && i <= 29 -> do
				State.put . ExtraBits $ (i - 2) `div` 2
				putDist t dt ln i
			| otherwise -> error $ "putDist: yet " ++ show i
		Right eb -> do
			Pipe.yield (RunLength.LenDist ln (calcDist pri eb))
			State.put (t, t)
			putDecoded t dt 0

getCodeTable :: forall o effs . (
	Union.Member Pipe.P effs,
	Union.Member (State.S ExtraBits) effs,
	Union.Member Fail.F effs ) =>
	Int -> Int -> Eff.E effs (Either Int Word16) o [Int]
getCodeTable _pr 0 = pure []
getCodeTable pr n = Pipe.await >>= \case
	Left ln
		| 0 <= ln && ln <= 15 -> (ln :) <$> getCodeTable @o ln (n - 1)
		| ln == 16 -> do
			State.put $ ExtraBits 2
			Right eb <- Pipe.await
			(replicate (fromIntegral eb + 3) pr ++) <$> getCodeTable @o pr (n - fromIntegral eb - 3)
		| ln == 17 -> do
			State.put $ ExtraBits 3
			Right eb <- Pipe.await
			(replicate (fromIntegral eb + 3) 0 ++) <$> getCodeTable @o 0 (n - fromIntegral eb - 3)
		| ln == 18 -> do
			State.put $ ExtraBits 7
			Right eb <- Pipe.await
			(replicate (fromIntegral eb + 11) 0 ++) <$> getCodeTable @o 0 (n - fromIntegral eb - 11)
		| otherwise -> error "yet"
	Right _ -> error "bad"

skipLeft1 :: -- forall effs . forall o -> forall a -> forall b -> (
	(
	Union.Member Pipe.P effs,
	Union.Member (Except.E String) effs ) =>
	Eff.E effs (Either a b) o b
skipLeft1 = Pipe.await >>= \case
	Left _ -> Pipe.await >>= \case
		Left _ -> Except.throw @String "Not Right"
		Right x -> pure x
	Right x -> pure x

getWord16FromPair :: (
	Union.Member (Except.E String) effs,
	Integral n
	) =>
	BS.ByteString -> Eff.E effs i o n
getWord16FromPair bs0 = fromIntegral @Word16 <$> do
	when (BS.length bs0 /= 4)
		$ Except.throw @String "getWord16FromPair: not 4 bytes"
	when (ln /= complement cln)
		$ Except.throw @String "bad pair"
	pure ln
	where
	(ln, cln) = (tow16 *** tow16) $ BS.splitAt 2 bs0
	tow16 bs = case BS.unpack bs of
		[b0, b1] -> fromIntegral b0 .|. (fromIntegral b1) `shiftL` 8
		_ -> error "never occur bar"

separate :: Int -> Int -> [Int]
separate bs ln
	| ln == 0 = [] | ln <= bs = [ln]
	| otherwise = bs : separate bs (ln - bs)

getRight :: Union.Member (Except.E String) effs => Either a b -> Eff.E effs i o b
getRight = \case
	Left _ -> Except.throw @String "No Right"
	Right x -> pure x

getLeft :: Union.Member (Except.E String) effs => Either a b -> Eff.E effs i o a
getLeft = \case
	Left x -> pure x
	Right _ -> Except.throw @String "Not Left"

format :: (
	Union.Member Pipe.P effs,
	Union.Member (State.Named "format" BS.ByteString) effs ) =>
	Int -> Eff.E effs (Either Word8 BS.ByteString) BS.ByteString ()
format n = do
	b <- checkLength n
	if b
	then yieldLen n >> format n
	else readMore2 >>= bool
		(Pipe.yield =<<
			State.getN @BS.ByteString "format")
		(format n)

readMore2 :: (
	Union.Member (State.Named "format" BS.ByteString) effs,
	Union.Member Pipe.P effs ) =>
	Eff.E effs (Either Word8 BS.ByteString) BS.ByteString Bool
readMore2 = do
	e <- Pipe.isEmpty
	if e then pure False else Pipe.await >>= \case
		Left w -> True <$ State.modifyN "format" (`BS.snoc` w)
		Right bs -> True <$ State.modifyN "format" (`BS.append` bs)

checkLength :: (Union.Member (State.Named "format" BS.ByteString) effs) =>
	Int -> Eff.E effs i o Bool
checkLength n = do
	bs <- State.getN "format"
	pure $ BS.length bs >= n

yieldLen :: forall i effs . (
	Union.Member Pipe.P effs,
	Union.Member (State.Named "format" BS.ByteString) effs ) =>
	Int -> Eff.E effs i BS.ByteString ()
yieldLen n = do
	bs <- State.getN "format"
	let	(r, bs') = BS.splitAt n bs
	State.putN "format" bs'
	Pipe.yield r
