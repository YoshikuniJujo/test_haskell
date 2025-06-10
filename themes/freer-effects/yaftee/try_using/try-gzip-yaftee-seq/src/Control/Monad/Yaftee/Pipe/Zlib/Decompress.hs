{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ExplicitForAll, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.Zlib.Decompress (

	run_, States,

	decompress, Members, Sequence

	) where

import GHC.TypeLits
import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.BitArray.OnDemand qualified as OnDemand
import Control.Monad.Yaftee.Pipe.Sequence.Adler32 qualified as PipeAdler32
import Control.Monad.Yaftee.Pipe.Deflate.Decompress qualified as Deflate
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.Fail qualified as Fail
import Control.HigherOpenUnion qualified as U
import Data.TypeLevel.List
import Data.Foldable
import Data.HigherFunctor qualified as HFunctor
import Data.Sequence qualified as Seq
import Data.Sequence.ToolsYj qualified as Seq
import Data.Sequence.Word8 qualified as Seq
import Data.Sequence.BitArray qualified as BitArray
import Data.Word
import Data.Word.Adler32 qualified as Adler32
import Data.Zlib qualified as Zlib

run_ :: HFunctor.Loose (U.U es) =>
	Eff.E (States "foobar" `Append` es) i o r ->
	Eff.E es i o ()
run_ = void
	. flip (State.runN @"foobar") (Sequence Seq.empty)
	. flip (State.runN @"foobar") Adler32.initial
	. Deflate.run_ @"foobar"

type States nm = Deflate.States nm `Append` '[
	State.Named nm (Int, Adler32.A),
	State.Named nm Sequence ]

decompress :: forall nm -> (
	U.Member Pipe.P es, Members nm es,
	U.Member (State.Named nm OnDemand.Request) es,
	U.Member (Except.E String) es, U.Member Fail.F es) =>
	(Zlib.Header ->
		Eff.E es (Either BitArray.B (Seq.Seq Word8)) (Seq.Seq Word8) r) ->
	[Int] ->
	Eff.E es (Either BitArray.B (Seq.Seq Word8)) (Seq.Seq Word8) ()
decompress nm f rs = do
	State.putN nm $ OnDemand.RequestBytes 2
	_ <- f =<< zlibHeader =<< Except.getRight @String "bad" =<< Pipe.await
	State.putN nm $ OnDemand.RequestBuffer 500
	_ <- Deflate.decompress nm Pipe.=$=
		format nm rs Pipe.=$= PipeAdler32.adler32 nm
	State.putN nm $ OnDemand.RequestBytes 4
	adl1 <- Adler32.toWord32 . snd <$> State.getN @(Int, Adler32.A) nm
	adl0 <- Seq.toBitsBE <$> PipeT.skipLeft1
	when (adl1 /= adl0) $ Except.throw @String "ADLER-32 error"

type Members nm es = (
	Deflate.Members nm es,
	U.Member (State.Named nm Sequence) es,
	U.Member (State.Named nm (Int, Adler32.A)) es )

zlibHeader :: (U.Member (Except.E String) es, U.Member Fail.F es) =>
	Seq.Seq Word8 -> Eff.E es i o Zlib.Header
zlibHeader bs = do
	[cmf, flg] <- pure $ toList bs
	maybe (Except.throw @String "Zlib header check bits error")
		pure (Zlib.readHeader cmf flg)

newtype Sequence = Sequence { unSequence :: Seq.Seq Word8 } deriving Show

format :: forall (nm :: Symbol) -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm Sequence) es,
	U.Member Fail.F es ) =>
	[Int] -> Eff.E es (Either Word8 (Seq.Seq Word8)) (Seq.Seq Word8) ()
format nm = \case
	[] -> do { Right Seq.Empty <- Pipe.await; pure () }
	r : rs -> (Pipe.yield =<< getBytes nm r) >> format nm rs

getBytes :: forall (nm :: Symbol) ->
	(U.Member Pipe.P es, U.Member (State.Named nm Sequence) es) =>
	Int -> Eff.E es (Either Word8 (Seq.Seq Word8)) o (Seq.Seq Word8)
getBytes nm n = State.getsN nm (Seq.splitAt' n . unSequence) >>= \case
	Nothing -> readMore nm >> getBytes nm n
	Just (t, d) -> t <$ State.putN nm (Sequence d)

readMore :: forall (nm :: Symbol) ->
	(U.Member Pipe.P es, U.Member (State.Named nm Sequence) es) =>
	Eff.E es (Either Word8 (Seq.Seq Word8)) o ()
readMore nm = Pipe.await >>= \case
	Left w -> State.modifyN nm $ Sequence . (Seq.:|> w) . unSequence
	Right s -> State.modifyN nm $ Sequence . (Seq.>< s) . unSequence
