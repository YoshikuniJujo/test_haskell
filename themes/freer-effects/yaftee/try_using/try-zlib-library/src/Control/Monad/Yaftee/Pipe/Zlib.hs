{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.Zlib (

	run, deflate, DeflateOptions(..), inflate,

	ByteString, CByteArray, cByteArrayMalloc, cByteArrayFree

	) where

import Foreign.Ptr
import Foreign.C.String
import Foreign.C.ByteArray qualified as CByteArray
import Control.Monad
import Control.Monad.ToolsYj
import Control.Monad.Primitive
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.IO qualified as IO
import Control.HigherOpenUnion qualified as U
import Data.HigherFunctor qualified as HFunctor

import Codec.Compression.Zlib.Structure.Core qualified as Zlib
import Codec.Compression.Zlib.Constant.Core qualified as Zlib
import Codec.Compression.Zlib.Basic.Core qualified as Zlib
import Codec.Compression.Zlib.Advanced.Core qualified as Zlib

import Data.ByteString qualified as BS
import Data.ByteString.FingerTree qualified as BSF
import Data.ByteString.FingerTree.CString qualified as BSF

import Debug.Trace

run :: forall nm es i o r . HFunctor.Loose (U.U es) =>
	Eff.E (State.Named nm (Maybe ByteString) ': es) i o r ->
	Eff.E es i o (r, Maybe ByteString)
run = (`State.runN` (Nothing :: Maybe ByteString))

newtype ByteString = ByteString { unByteString :: BSF.ByteString } deriving Show

newtype CByteArray s = CByteArray { unCByteArray :: CByteArray.B } deriving Show

cByteArrayMalloc :: PrimMonad m => Int -> m (CByteArray (PrimState m))
cByteArrayMalloc n = unsafeIOToPrim $ CByteArray <$> CByteArray.malloc n

cByteArrayFree :: PrimMonad m => CByteArray (PrimState m) -> m ()
cByteArrayFree (CByteArray ba) = unsafeIOToPrim $ CByteArray.free ba

data DeflateOptions = DeflateOptions {
	deflateOptionsCompressionLevel :: Zlib.CompressionLevel,
	deflateOptionsCompressionMethod :: Zlib.CompressionMethod,
	deflateOptionsWindowBits :: Zlib.WindowBits,
	deflateOptionsMemLevel :: Zlib.MemLevel,
	deflateOptionsCompressionStrategy :: Zlib.CompressionStrategy }
	deriving Show

deflate :: forall nm m -> (
	PrimBase m,
	U.Member Pipe.P es, U.Member (State.Named nm (Maybe ByteString)) es,
	U.Member (Except.E Zlib.ReturnCode) es, U.Base (U.FromFirst m) es ) =>
	DeflateOptions ->
	CByteArray (PrimState m) -> CByteArray (PrimState m) ->
	Eff.E es BSF.ByteString BSF.ByteString BSF.ByteString
deflate nm m dos bai bao = do
	strm <- initialize nm m
		(\s os -> Zlib.deflateInit2 s
			(deflateOptionsCompressionLevel os)
			(deflateOptionsCompressionMethod os)
			(deflateOptionsWindowBits os)
			(deflateOptionsMemLevel os)
			(deflateOptionsCompressionStrategy os))
		dos bai bao
	doWhile_ $ body nm m Zlib.deflate bai bao strm
	finalize m Zlib.deflateEnd strm
	restOfInput nm m strm

inflate :: forall nm m -> (
	PrimBase m,
	U.Member Pipe.P es, U.Member (State.Named nm (Maybe ByteString)) es,
	U.Member (Except.E Zlib.ReturnCode) es, U.Base (U.FromFirst m) es ) =>
	Zlib.WindowBits ->
	CByteArray (PrimState m) -> CByteArray (PrimState m) ->
	Eff.E es BSF.ByteString BSF.ByteString BSF.ByteString
inflate nm m wbs bai bao = do
	strm <- initialize nm m Zlib.inflateInit2 wbs bai bao
	doWhile_ $ body nm m Zlib.inflate bai bao strm
	finalize m Zlib.inflateEnd strm
	restOfInput nm m strm

initialize :: forall nm m -> (
	PrimBase m,
	U.Member Pipe.P es, U.Member (State.Named nm (Maybe ByteString)) es,
	U.Member (Except.E Zlib.ReturnCode) es, U.Base (U.FromFirst m) es ) =>
	(forall m' . PrimBase m' =>
		Zlib.StreamPrim (PrimState m') -> arg -> m' Zlib.ReturnCode) ->
	arg -> CByteArray (PrimState m) -> CByteArray (PrimState m) ->
	Eff.E es BSF.ByteString BSF.ByteString (Zlib.StreamPrim (PrimState m))
initialize nm m f a
	(CByteArray (castPtr -> i, ni))
	(CByteArray (o, (fromIntegral -> no'))) = do
	bs0 <- awaitInput
	((castPtr -> i0, fromIntegral -> n0), ebs0) <-
		Eff.effBase . unsafeIOToPrim @m $ BSF.poke (i, ni) bs0
	either (const $ pure ()) (putInput nm ByteString) ebs0
	strm <- Eff.effBase $ Zlib.streamThaw @m Zlib.streamInitial {
		Zlib.streamNextIn = i0, Zlib.streamAvailIn = n0,
		Zlib.streamNextOut = o, Zlib.streamAvailOut = no' }
	rc0 <- Eff.effBase $ f @m strm a
	when (rc0 /= Zlib.Ok) $ Except.throw rc0
	pure strm

awaitInput :: U.Member Pipe.P es => Eff.E es BSF.ByteString o BSF.ByteString
awaitInput = do
	bs <- Pipe.await
	if BSF.null bs then awaitInput else pure bs

awaitInputMaybe :: U.Member Pipe.P es => (i -> Bool) -> Eff.E es i o (Maybe i)
awaitInputMaybe p = Pipe.awaitMaybe >>= \case
	Nothing -> pure Nothing
	Just bs -> if p bs then awaitInputMaybe p else pure $ Just bs

finalize :: forall m -> (
	PrimMonad m,
	U.Member (Except.E Zlib.ReturnCode) es, U.Base (U.FromFirst m) es ) =>
	(Zlib.StreamPrim (PrimState m) -> m Zlib.ReturnCode) ->
	Zlib.StreamPrim (PrimState m) -> Eff.E es i o ()
finalize m f strm = do
	rc <- Eff.effBase $ f strm
	trace "FINALIZE" $ pure ()
	when (rc /= Zlib.Ok) do
--		cmsg <- Eff.effBase $ Zlib.msg @m strm
--		msg <- Eff.effBase . unsafeIOToPrim @m $ peekCString cmsg
--		trace msg $ pure ()
		Except.throw rc

body :: forall nm m -> (
	PrimBase m,
	U.Member Pipe.P es,
	U.Member (State.Named nm (Maybe ByteString)) es,
	U.Member (Except.E Zlib.ReturnCode) es,
	U.Base (U.FromFirst m) es ) =>
	(forall m' . PrimBase m' => Zlib.StreamPrim (PrimState m') -> Zlib.Flush -> m' Zlib.ReturnCode) ->
	CByteArray (PrimState m) -> CByteArray (PrimState m) ->
	Zlib.StreamPrim (PrimState m) ->
	Eff.E es BSF.ByteString BSF.ByteString Bool
body nm m f
	(CByteArray (castPtr -> i, ni))
	(CByteArray (o@(castPtr -> o'), no@(fromIntegral -> no')))
	strm = do
		rc <- Eff.effBase $ f @m strm Zlib.NoFlush
--		rc <- Eff.effBase $ f @m strm Zlib.FullFlush
		ai <- Eff.effBase @m $ Zlib.availIn strm
		(fromIntegral -> ao) <- Eff.effBase @m $ Zlib.availOut strm
		when (rc `notElem` [Zlib.Ok, Zlib.StreamEnd]) do
			cmsg <- Eff.effBase $ Zlib.msg @m strm
			msg <- Eff.effBase . unsafeIOToPrim @m $ peekCString cmsg
			trace msg $ pure ()
			Except.throw rc
		Pipe.yield =<< Eff.effBase
			(unsafeIOToPrim @m $ BSF.peek (o', no - ao))
		Eff.effBase $ Zlib.setNextOut @m strm o no'
		whenDef (rc /= Zlib.StreamEnd) (rc /= Zlib.StreamEnd && ai == 0) do
			minp <- getInput nm BSF.null unByteString
			case minp of
				Nothing -> do
					doWhile_ do
						rc <- Eff.effBase $ f @m strm Zlib.Finish
						(fromIntegral -> ao') <- Eff.effBase @m $ Zlib.availOut strm
						Pipe.yield =<< Eff.effBase
							(unsafeIOToPrim @m $ BSF.peek (o', no - ao'))
						trace (show rc) $ pure ()
						Eff.effBase $ Zlib.setNextOut @m strm o no'
						pure $ rc /= Zlib.StreamEnd
					pure False
				Just inp -> do
					((castPtr -> i', fromIntegral -> n), ebs) <- Eff.effBase
						. unsafeIOToPrim @m
						$ BSF.poke (i, ni) inp
					either (const $ pure ()) (putInput nm ByteString) ebs
					Eff.effBase $ Zlib.setNextIn @m strm i' n
					pure $ rc /= Zlib.StreamEnd

restOfInput :: forall nm m -> (
	PrimBase m,
	U.Member (State.Named nm (Maybe ByteString)) es,
	U.Base (U.FromFirst m) es ) =>
	Zlib.StreamPrim (PrimState m) ->
	Eff.E es i o BSF.ByteString
restOfInput nm m strm = do
	(castPtr -> i') <- Eff.effBase @m $ Zlib.nextIn strm
	(fromIntegral -> ai) <- Eff.effBase @m $ Zlib.availIn strm
	rbs <- Eff.effBase @m . unsafeIOToPrim $ BS.packCStringLen (i', ai)
	ByteString bs' <- maybe (ByteString "") id <$> State.getN nm
	pure $ rbs BSF.:<| bs'

getInput :: forall es i' i o . forall nm ->
	(U.Member Pipe.P es, U.Member (State.Named nm (Maybe i')) es) =>
	(i -> Bool) -> (i' -> i) -> Eff.E es i o (Maybe i)
getInput nm p un = State.getN nm
	>>= maybe (awaitInputMaybe p) ((<$ State.putN @(Maybe i') nm Nothing) . Just . un)

putInput :: forall nm -> (U.Member (State.Named nm (Maybe i')) es) =>
	(i -> i') -> i -> Eff.E es i o ()
putInput nm to = State.putN nm . Just . to
