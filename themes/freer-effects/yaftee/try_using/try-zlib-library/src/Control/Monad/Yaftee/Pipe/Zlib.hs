{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.Zlib (

	inflateRun, inflate,

	ByteString, CByteArray, cByteArrayMalloc, cByteArrayFree

	) where

import Foreign.Ptr
import Foreign.C.ByteArray qualified as CByteArray
import Control.Monad
import Control.Monad.ToolsYj
import Control.Monad.Primitive
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.HigherOpenUnion qualified as U
import Data.HigherFunctor qualified as HFunctor

import Codec.Compression.Zlib.Structure.Core qualified as Zlib
import Codec.Compression.Zlib.Constant.Core qualified as Zlib
import Codec.Compression.Zlib.Basic.Core qualified as Zlib
import Codec.Compression.Zlib.Advanced.Core qualified as Zlib

import Data.ByteString qualified as BS
import Data.ByteString.FingerTree qualified as BSF
import Data.ByteString.FingerTree.CString qualified as BSF

inflateRun :: forall nm es i o r . HFunctor.Loose (U.U es) =>
	Eff.E (State.Named nm (Maybe ByteString) ': es) i o r ->
	Eff.E es i o (r, Maybe ByteString)
inflateRun = (`State.runN` (Nothing :: Maybe ByteString))

newtype ByteString = ByteString { unByteString :: BSF.ByteString } deriving Show

newtype CByteArray s = CByteArray { unCByteArray :: CByteArray.B } deriving Show

cByteArrayMalloc :: PrimMonad m => Int -> m (CByteArray (PrimState m))
cByteArrayMalloc n = unsafeIOToPrim $ CByteArray <$> CByteArray.malloc n

cByteArrayFree :: PrimMonad m => CByteArray (PrimState m) -> m ()
cByteArrayFree (CByteArray ba) = unsafeIOToPrim $ CByteArray.free ba

inflate :: forall nm m -> (
	PrimBase m,
	U.Member Pipe.P es, U.Member (State.Named nm (Maybe ByteString)) es,
	U.Member (Except.E Zlib.ReturnCode) es, U.Base (U.FromFirst m) es ) =>
	Zlib.WindowBits -> CByteArray (PrimState m) -> CByteArray (PrimState m) ->
	Eff.E es BSF.ByteString BSF.ByteString BSF.ByteString
inflate nm m wbs
	(CByteArray (castPtr -> i, ni))
	(CByteArray (o@(castPtr -> o'), no@(fromIntegral -> no'))) = do
	bs0 <- Pipe.await
	((castPtr -> i0, fromIntegral -> n0), ebs0) <-
		Eff.effBase . unsafeIOToPrim @m $ BSF.poke (i, ni) bs0
	either (const $ pure ()) (putInput nm ByteString) ebs0
	strm <- Eff.effBase $ Zlib.streamThaw @m Zlib.streamInitial {
		Zlib.streamNextIn = i0, Zlib.streamAvailIn = n0,
		Zlib.streamNextOut = o, Zlib.streamAvailOut = no' }
	rc0 <- Eff.effBase $ Zlib.inflateInit2 @m strm wbs
	when (rc0 /= Zlib.Ok) $ Except.throw rc0

	doWhile_ do
		rc <- Eff.effBase $ Zlib.inflate @m strm Zlib.NoFlush
		ai <- Eff.effBase @m $ Zlib.availIn strm
		(fromIntegral -> ao) <- Eff.effBase @m $ Zlib.availOut strm
		when (rc `notElem` [Zlib.Ok, Zlib.StreamEnd]) $ Except.throw rc
		Pipe.yield =<< Eff.effBase
			(unsafeIOToPrim @m $ BSF.peek (o', no - ao))
		Eff.effBase $ Zlib.setNextOut @m strm o no'
		when (rc /= Zlib.StreamEnd && ai == 0) do
			((castPtr -> i', fromIntegral -> n), ebs) <- Eff.effBase
				. unsafeIOToPrim @m
				. BSF.poke (i, ni) =<< getInput nm unByteString
			either (const $ pure ()) (putInput nm ByteString) ebs
			Eff.effBase $ Zlib.setNextIn @m strm i' n
		pure $ rc /= Zlib.StreamEnd

	rc <- Eff.effBase $ Zlib.inflateEnd @m strm
	when (rc /= Zlib.Ok) $ Except.throw rc

	(castPtr -> i') <- Eff.effBase @m $ Zlib.nextIn strm
	(fromIntegral -> ai) <- Eff.effBase @m $ Zlib.availIn strm
	rbs <- Eff.effBase @m . unsafeIOToPrim $ BS.packCStringLen (i', ai)
	ByteString bs' <- maybe (ByteString "") id <$> State.getN nm
	pure $ rbs BSF.:<| bs'

getInput :: forall es i' i o . forall nm ->
	(U.Member Pipe.P es, U.Member (State.Named nm (Maybe i')) es) =>
	(i' -> i) -> Eff.E es i o i
getInput nm un = State.getN nm
	>>= maybe Pipe.await ((<$ State.putN @(Maybe i') nm Nothing) . un)

putInput :: forall nm -> (U.Member (State.Named nm (Maybe i')) es) =>
	(i -> i') -> i -> Eff.E es i o ()
putInput nm to = State.putN nm . Just . to
