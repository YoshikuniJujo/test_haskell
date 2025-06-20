{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-x-partial #-}

module Pipe.Runlength.Compress (

	run_, States,
	compress, Members,

	AheadPos, Sequence

	) where

import Control.Monad
import Control.Monad.Fix
import Data.TypeLevel.List (Append)
import Data.Foldable
import Data.Bool
import Data.Word
import Data.Sequence qualified as Seq
import Data.Sequence.ToolsYj qualified as Seq

import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Pipe.Runlength qualified as RL
import Pipe.Runlength.Triple qualified as Triple
import Control.Monad.Yaftee.State qualified as State
import Data.HigherFunctor qualified as HFunctor
import Control.HigherOpenUnion qualified as U

run_ :: forall nm es i o a . HFunctor.Loose (U.U es) =>
	Eff.E (States nm `Append` es) i o a -> Eff.E es i o ()
run_ = void
	. (`State.runN` Sequence Seq.Empty)
	. (`State.runN` Triple.empty)
	. (`State.runN` AheadPos 0)

type States nm = '[
	State.Named nm AheadPos, State.Named nm Triple.T,
	State.Named nm Sequence ]

compress :: forall nm ->
	(U.Member Pipe.P es, Members nm es) => Eff.E es (Seq.Seq Word8) RL.R ()
compress nm = fix \go -> get3 nm >>= \(mb, mb1, mb2) ->
	($ mb) $ maybe (pure ()) \b -> (>> go) case (mb1, mb2) of
		(Just b1, Just b2) -> State.getN nm >>= \st ->
			Triple.indexLength' st b b1 b2 (getAhead nm) >>= \case
				Nothing -> do
					State.modifyN nm (`Triple.update` b)
					Pipe.yield (RL.Literal b)
				Just (i, l) -> putLenDist nm b b1 b2 i l
		_ -> do	State.modifyN nm (`Triple.update` b)
			Pipe.yield (RL.Literal b)

type Members nm es = (
	U.Member (State.Named nm AheadPos) es,
	U.Member (State.Named nm Triple.T) es,
	U.Member (State.Named nm Sequence) es )

putLenDist :: forall nm -> (
	U.Member Pipe.P es, U.Member (State.Named nm Triple.T) es,
	U.Member (State.Named nm AheadPos) es, U.Member (State.Named nm Sequence) es ) =>
	Word8 -> Word8 -> Word8 -> Int -> RL.Length ->
	Eff.E es (Seq.Seq Word8) RL.R ()
putLenDist nm b b1 b2 i0 l0 = State.getsN nm (Triple.distance i0) >>= \d0 -> do
	State.modifyN nm (`Triple.update` b)
	let	c1 = (l0 + 1, [RL.LenDist (l0 + 3) d0])
	(_mb1, _mb2, mb3) <- get3 nm
	case mb3 of
		Nothing -> State.modifyN nm (`Triple.update` b1) >> proceed nm c1
		Just b3 -> State.getN nm >>= \st ->
			Triple.indexLength' st b1 b2 b3 (getAhead nm) >>= \case
				Nothing -> do
					State.modifyN nm (`Triple.update` b1)
					proceed nm c1
				Just (i, l) -> do
					d <- State.getsN nm $ Triple.distance i
					State.modifyN nm (`Triple.update` b1)
					proceed nm if l0 >= l then c1 else
						(l + 2, [
							RL.Literal b,
							RL.LenDist (l + 3) d ])

proceed :: forall nm -> (
	Foldable t,
	U.Member Pipe.P es,
	U.Member (State.Named nm Triple.T) es, U.Member (State.Named nm AheadPos) es,
	U.Member (State.Named nm Sequence) es ) =>
	(Int, t o) -> Eff.E es (Seq.Seq Word8) o ()
proceed nm (l', ys) = getBytes nm l' >>= \bs -> do
	State.modifyN nm \st -> foldl Triple.update st $ toList bs
	Pipe.yield `mapM_` ys

get3 :: forall nm -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm AheadPos) es, U.Member (State.Named nm Sequence) es ) =>
	Eff.E es (Seq.Seq Word8) o (Maybe Word8, Maybe Word8, Maybe Word8)
get3 nm = (,,) <$> get nm <*> getAhead nm <*> getAhead nm

get :: forall nm -> (
	U.Member Pipe.P es, U.Member (State.Named nm AheadPos) es,
	U.Member (State.Named nm Sequence) es ) =>
	Eff.E es (Seq.Seq Word8) o (Maybe Word8)
get nm = State.getsN nm (Seq.uncons . unSequence) >>= \case
	Nothing -> bool (pure Nothing) (get nm) =<< readMore nm
	Just (b, bs) ->
		Just b <$ (State.putN nm (AheadPos 0) >> State.putN nm (Sequence bs))

getBytes :: forall nm -> (
	U.Member Pipe.P es, U.Member (State.Named nm AheadPos) es,
	U.Member (State.Named nm Sequence) es ) =>
	Int -> Eff.E es (Seq.Seq Word8) o (Seq.Seq Word8)
getBytes nm n = State.getsN nm unSequence >>= \bs -> if length bs >= n
	then Seq.take n bs <$ State.putN nm (Sequence $ Seq.drop n bs)
	else readMore nm >> getBytes nm n

getAhead :: forall nm -> (
	U.Member Pipe.P es, U.Member (State.Named nm AheadPos) es,
	U.Member (State.Named nm Sequence) es ) =>
	Eff.E es (Seq.Seq Word8) o (Maybe Word8)
getAhead nm = State.getN nm >>= \(Sequence bs) -> State.getN nm >>= \(AheadPos i) ->
	case bs Seq.!? i of
		Nothing -> bool (pure Nothing) (getAhead nm) =<< readMore nm
		Just b -> Just b <$ State.modifyN nm nextAheadPos

newtype AheadPos = AheadPos Int deriving Show

nextAheadPos :: AheadPos -> AheadPos
nextAheadPos (AheadPos p) = AheadPos $ p + 1

readMore :: forall nm ->
	(U.Member Pipe.P es, U.Member (State.Named nm Sequence) es) =>
	Eff.E es (Seq.Seq Word8) o Bool
readMore nm = Pipe.isMore >>= bool (pure False)
	(True <$ (State.modifyN nm . flip append =<< Pipe.await))

newtype Sequence = Sequence { unSequence :: Seq.Seq Word8 } deriving Show

append :: Sequence -> Seq.Seq Word8 -> Sequence
bs1 `append` bs2 = Sequence $ unSequence bs1 Seq.>< bs2
