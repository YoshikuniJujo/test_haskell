{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.Png.Chunk (

	-- * DATA TYPE

	C(..),

	-- * DECODE

	decode, hDecode, decodeRun_, DecodeStates,

	-- * ENCODE

	encode, hEncode, encodeRun_, EncodeStates

	) where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeBS
import Control.Monad.Yaftee.Pipe.ByteString.FingerTree.OnDemand
	qualified as OnDemand
import Control.Monad.Yaftee.Pipe.MonoTraversable.Crc32
	qualified as PipeCrc32
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.Fail qualified as Fail
import Control.Monad.Yaftee.IO qualified as IO
import Control.HigherOpenUnion qualified as U
import Data.Foldable
import Data.MonoTraversable
import Data.Word
import Data.Word.Word8 qualified as Word8
import Data.Word.Crc32 qualified as Crc32
import Data.ByteString.FingerTree qualified as BSF
import Data.ByteString.FingerTree.Bits qualified as BSF
import System.IO	

import Data.HigherFunctor qualified as F
import Data.TypeLevel.List

import Numeric


decode :: forall nm -> (
	U.Member Pipe.P es, OnDemand.Members nm es,
	U.Member (State.Named nm Crc32.C) es, U.Member (Except.E String) es ) =>
	Int -> Eff.E es BSF.ByteString C ()
decode nm n = void $ OnDemand.onDemand nm Pipe.=$= PipeCrc32.crc32 nm Pipe.=$= do
	State.putN nm $ OnDemand.RequestBytes 8
	fh <- Pipe.await
	when (fh /= fileHeader) . Except.throw @String
		$ "Not PNG file: " ++ show fh
	chunks nm n

hDecode :: forall nm -> (
	U.Member Pipe.P es, OnDemand.Members nm es,
	U.Member (State.Named nm Crc32.C) es, U.Member (Except.E String) es,
	U.Base IO.I es ) =>
	Handle -> Int -> Int -> Eff.E es BSF.ByteString C ()
hDecode nm h n n' = void $
	PipeBS.hGet n h Pipe.=$=
	PipeT.convert BSF.fromStrict Pipe.=$=
	decode nm n'

chunks :: forall nm -> (
	U.Member Pipe.P es, OnDemand.Members nm es,
	U.Member (State.Named nm Crc32.C) es, U.Member (Except.E String) es ) =>
	Int -> Eff.E es BSF.ByteString C ()
chunks nm d = fix \go -> do
	b <- chunk1 nm d
	if b then go else Pipe.yield EndOfTheWorld

chunk1 :: forall nm -> (
	U.Member Pipe.P es, OnDemand.Members nm es,
	U.Member (State.Named nm Crc32.C) es, U.Member (Except.E String) es ) =>
	Int -> Eff.E es BSF.ByteString C Bool
chunk1 nm d = do
	State.putN nm $ OnDemand.RequestBytes 4
	n <- Word8.toBitsBE @_ @Int <$> Pipe.await
	PipeCrc32.reset nm
	State.putN nm $ OnDemand.RequestBytes 4
	cn <- Pipe.await
--	trace ("chunk1: yield Begin: " ++ show cn) (Pipe.yield $ Begin n cn)
	Pipe.yield $ Begin n cn
	for_ (split d n) \n' -> do
		State.putN nm $ OnDemand.RequestBytes n'
		Pipe.yield . Body =<< Pipe.await
	PipeCrc32.complement nm
	c1 <- State.getN @Crc32.C nm
	State.putN nm $ OnDemand.RequestBytes 4
	c0 <- Crc32.fromWord . Word8.toBitsBE @_ @Word32 <$> Pipe.await
	when (c1 /= c0) $ Except.throw @String
		$ "corrupted -- crc32 mismatch: " ++
			showHex (Crc32.toWord c0) "" ++ " " ++ showHex (Crc32.toWord c1) ""
--	trace ("chunk1: yield End: " ++ show cn) $ Pipe.yield End
	Pipe.yield End
	pure $ cn /= "IEND"
	where
	split n = fix \go -> \case
		0 -> []
		m | n < m -> n : go (m - n) | otherwise -> [m]

decodeRun_ ::
	F.Loose (U.U es) =>
	Eff.E (DecodeStates nm `Append` es) i o r -> Eff.E es i o ()
decodeRun_ = void . (`State.runN` Crc32.initial)

type DecodeStates nm = '[State.Named nm Crc32.C]

data C	= Begin Int BSF.ByteString
	| Body BSF.ByteString | End
	| EndOfTheWorld
	deriving Show

fileHeader :: BSF.ByteString
fileHeader = "\137PNG\r\n\SUB\n"

type instance Element C = Word8

instance MonoFoldable C where
	ofoldMap f = \case
		Begin _ bs -> ofoldMap f bs
		Body bs -> ofoldMap f bs; _ -> mempty
	ofoldr o v = \case
		Begin _ bs -> ofoldr o v bs
		Body bs -> ofoldr o v bs; _ -> v
	ofoldl' o v = \case
		Begin _ bs -> ofoldl' o v bs
		Body bs -> ofoldl' o v bs; _ -> v
	ofoldr1Ex o = \case
		Begin _ bs -> ofoldr1Ex o bs
		Body bs -> ofoldr1Ex o bs; _ -> error "instance MonoFoldable C: bad"
	ofoldl1Ex' o = \case
		Begin _ bs -> ofoldl1Ex' o bs
		Body bs -> ofoldl1Ex' o bs; _ -> error "instance MonoFoldable C: bad"

encode :: forall nm -> (
	U.Member Pipe.P es, U.Member (State.Named nm Crc32.C) es,
	U.Member (Except.E String) es, U.Member Fail.F es ) =>
	Eff.E es C BSF.ByteString ()
encode nm = void $
	PipeCrc32.crc32 nm
	Pipe.=$= do
		Pipe.yield fileHeader
		fix \go -> (`when` go) =<< encodeChunk1 nm

hEncode :: forall nm -> (
	U.Member Pipe.P es, U.Member (State.Named nm Crc32.C) es,
	U.Member (Except.E String) es, U.Member Fail.F es, U.Base IO.I es ) =>
	Handle -> Eff.E es C o ()
hEncode nm ho = void $ encode nm
	Pipe.=$= PipeT.convert BSF.toStrict
	Pipe.=$= PipeBS.hPutStr ho

encodeChunk1 :: forall nm -> (
	U.Member Pipe.P es, U.Member (State.Named nm Crc32.C) es,
	U.Member (Except.E String) es, U.Member Fail.F es ) =>
	Eff.E es C BSF.ByteString Bool
encodeChunk1 nm = do
	PipeCrc32.reset nm
	Begin n cn <- Pipe.await
	Pipe.yield . BSF.fromBitsBE' @Word32 $ fromIntegral n
	Pipe.yield cn
	fix \go -> Pipe.await >>= \case
		Body bd -> Pipe.yield bd >> go
		End -> pure ()
		_ -> Except.throw @String "encodeChunk1: bad"
	PipeCrc32.complement nm
	c <- State.getN @Crc32.C nm
	Pipe.yield . BSF.fromBitsBE' $ Crc32.toWord c
	pure $ cn /= "IEND"

encodeRun_ :: forall nm es i o r .
	F.Loose (U.U es) =>
	Eff.E (EncodeStates nm `Append` es) i o r ->
	Eff.E es i o ()
encodeRun_ = void . OnDemand.run @nm . PipeCrc32.run @nm

type EncodeStates nm = State.Named nm Crc32.C ': OnDemand.States nm
