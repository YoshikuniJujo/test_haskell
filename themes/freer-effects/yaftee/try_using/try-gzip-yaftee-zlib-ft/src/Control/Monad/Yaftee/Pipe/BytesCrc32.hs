{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExplicitForAll, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.BytesCrc32 (
	bytesRun_, BytesStates,
	readBytes, BytesMembers, Sequence(..),
	resetCrc32, compCrc32, Crc32(..)
	) where

import GHC.TypeLits

import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.MonoTraversable.Crc32 qualified as PipeCrc32
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.HigherOpenUnion qualified as U

import Data.TypeLevel.List
import Data.HigherFunctor qualified as HFunctor
import Data.Bool
import Data.Word.Crc32 qualified as Crc32
import Data.ByteString.FingerTree qualified as BSF

bytesRun_ :: forall nm es i o r . HFunctor.Loose (U.U es) =>
	Eff.E (BytesStates nm `Append` es) i o r -> Eff.E es i o ()
bytesRun_ = void
	. (`State.runN` Crc32 Crc32.initial)
	. (`State.runN` Sequence BSF.empty)

type BytesStates nm = '[State.Named nm Sequence, State.Named nm Crc32]

readBytes :: forall (nm :: Symbol) -> (
	U.Member Pipe.P es, BytesMembers nm es,
	U.Member (Except.E String) es ) =>
	Int -> Eff.E es BSF.ByteString o BSF.ByteString
readBytes nm n = State.getsN nm (BSF.splitAt' n . unSequence) >>= \case
	Nothing -> readMore nm
		>>= bool (Except.throw @String "no more ByteString") (readBytes nm n)
	Just (t, d) -> t <$ do
		State.modifyN nm $ Crc32 . (`PipeCrc32.step` t) . unCrc32
		State.putN nm (Sequence d)

resetCrc32 :: forall (nm :: Symbol) ->
	(U.Member (State.Named nm Crc32) es) => Eff.E es i o ()
resetCrc32 nm = State.putN nm $ Crc32 Crc32.initial

compCrc32 :: forall (nm :: Symbol) ->
	(U.Member (State.Named nm Crc32) es) => Eff.E es i o ()
compCrc32 nm = State.modifyN nm $ Crc32 . Crc32.complement . unCrc32

type BytesMembers nm es = (
	U.Member (State.Named nm Sequence) es,
	U.Member (State.Named nm Crc32) es )

readMore :: forall (nm :: Symbol) -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm Sequence) es ) =>
	Eff.E es BSF.ByteString o Bool
readMore nm = Pipe.awaitMaybe >>= \case
	Nothing -> pure False
	Just bs -> True <$ State.modifyN nm (`appendSequence` bs)

appendSequence :: Sequence -> BSF.ByteString -> Sequence
appendSequence (Sequence bs1) bs2 = Sequence $ bs1 `BSF.append` bs2

newtype Sequence = Sequence { unSequence :: BSF.ByteString } deriving Show
newtype Crc32 = Crc32 { unCrc32 :: Crc32.C } deriving Show
