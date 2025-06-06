{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.BitArray.OnDemand (

	-- * RUN

	run_, States,

	-- * ON DEMAND

	onDemand, onDemandWithInitial, Members, Request(..), BitArray

	) where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.HigherOpenUnion qualified as U
import Data.TypeLevel.List
import Data.HigherFunctor qualified as HFunctor
import Data.Sequence qualified as Seq
import Data.Sequence.ToolsYj qualified as Seq
import Data.Sequence.BitArray qualified as BitArray
import Data.Bool
import Data.Word

run_ :: forall nm es i o a . HFunctor.Loose (U.U es) =>
	Eff.E (States nm `Append` es) i o a -> Eff.E es i o ()
run_ = void
	. (flip (State.runN @nm) $ BitArray BitArray.empty)
	. (flip (State.runN @nm) $ RequestBuffer 100)

type States nm = '[State.Named nm Request, State.Named nm BitArray]

onDemand :: forall nm -> (
	U.Member Pipe.P es,
	Members nm es, U.Member (Except.E String) es ) =>
	Eff.E es (Seq.Seq Word8) (Either BitArray.B (Seq.Seq Word8)) r
onDemand nm = fix \go -> State.getN nm >>= (yield go =<<) . \case
	RequestBits ln -> takeBits nm ln
	RequestBytes ln -> takeBytes nm ln
	RequestBuffer ln -> takeBuffer nm ln
	RequestString -> takeString nm
	RequestPushBack ba -> (Just $ Right Seq.Empty)
		<$ State.modifyN
			nm (BitArray . (ba `BitArray.append`) . unBitArray)
	where
	errne :: String
	errne = "Not enough Sequence elems"
	yield go = maybe (Except.throw errne) ((>> go) . Pipe.yield)

onDemandWithInitial :: forall nm ->
	(U.Member Pipe.P es, Members nm es, U.Member (Except.E String) es) =>
	Seq.Seq Word8 ->
	Eff.E es (Seq.Seq Word8) (Either BitArray.B (Seq.Seq Word8)) r
onDemandWithInitial nm ib = do
	State.putN nm . BitArray $ BitArray.fromSequence ib
	onDemand nm

type Members nm es = (
	U.Member (State.Named nm Request) es,
	U.Member (State.Named nm BitArray) es )

data Request
	= RequestBits Int | RequestBytes Int | RequestBuffer Int | RequestString
	| RequestPushBack BitArray.B
	deriving Show

newtype BitArray = BitArray { unBitArray :: BitArray.B } deriving Show

takeBits :: forall nm -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm BitArray) es ) =>
	Int -> Eff.E es (Seq.Seq Word8) o
		(Maybe (Either BitArray.B (Seq.Seq Word8)))
takeBits nm ln =
	State.getsN nm unBitArray >>= \ba -> case BitArray.splitAt ln ba of
		Nothing -> readMore nm >>= bool (pure Nothing) (takeBits nm ln)
		Just (t, d) -> Just (BitArray.toSequence t)
			<$ State.putN nm (BitArray d)

takeBytes :: forall nm -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm BitArray) es ) =>
	Int -> Eff.E es (Seq.Seq Word8) o
		(Maybe (Either BitArray.B (Seq.Seq Word8)))
takeBytes nm ln = State.getsN nm unBitArray >>= \ba ->
	case BitArray.byteBoundary ba of
		Left (t, d) -> Just (Left t) <$ State.putN nm (BitArray d)
		Right b -> case BitArray.toSequence b of
			Left _ -> error "bad"
			Right s -> case Seq.splitAt' ln s of
				Nothing -> readMore nm >>= bool (pure Nothing) (takeBytes nm ln)
				Just (t, d) -> Just (Right t)
					<$ State.putN nm (BitArray $ BitArray.fromSequence d)

takeBuffer :: forall nm -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm BitArray) es ) =>
	Int -> Eff.E es (Seq.Seq Word8) o
		(Maybe (Either BitArray.B (Seq.Seq Word8)))
takeBuffer nm ln = State.getsN nm unBitArray >>= \ba ->
	case BitArray.byteBoundary ba of
		Left (t, d) -> Just (Left t) <$ State.putN nm (BitArray d)
		Right b -> case BitArray.toSequence b of
			Left _ -> error "bad"
			Right s -> case Seq.splitAt' ln s of
				Nothing -> readMore nm >>= bool
					(bool	(Just (Right s) <$ State.putN nm (BitArray BitArray.empty))
						(pure Nothing) (Seq.null s))
					(takeBuffer nm ln)
				Just (t, d) -> Just (Right t)
					<$ State.putN nm (BitArray $ BitArray.fromSequence d)

takeString :: forall nm ->
	(U.Member Pipe.P es, U.Member (State.Named nm BitArray) es) =>
	Eff.E es (Seq.Seq Word8) o
		(Maybe (Either BitArray.B (Seq.Seq Word8)))
takeString nm = State.getsN nm unBitArray >>= \ba ->
	case BitArray.byteBoundary ba of
		Left (t, d) -> Just (Left t) <$ State.putN nm (BitArray d)
		Right b -> case BitArray.toSequence b of
			Left _ -> error "bad"
			Right bs -> case splitString bs of
				Nothing -> readMore nm >>= bool (pure Nothing) (takeString nm)
				Just (t, d) -> Just (Right t)
					<$ State.putN nm (BitArray $ BitArray.fromSequence d)

splitString :: Seq.Seq Word8 -> Maybe (Seq.Seq Word8, Seq.Seq Word8)
splitString s = case Seq.spanl (/= 0) s of
	(_, Seq.Empty) -> Nothing
	(t, Seq.uncons -> Just (z, d)) -> Just (Seq.snoc t z, d)
	_ -> error "Never occur"

readMore :: forall nm -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm BitArray) es ) =>
	Eff.E es (Seq.Seq Word8) o Bool
readMore nm = Pipe.awaitMaybe >>= \case
	Nothing -> pure False
	Just s -> True <$ State.modifyN nm
		(BitArray . (`BitArray.appendSequence` s) . unBitArray)
