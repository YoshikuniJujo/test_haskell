{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.ByteString.Lazy.OnDemand (

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
import Data.Bool
import Data.Int
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.ToolsYj qualified as LBS
import Data.ByteString.Lazy.BitArray qualified as BitArray

run_ :: forall nm es i o a . HFunctor.Loose (U.U es) =>
	Eff.E (States nm `Append` es) i o a -> Eff.E es i o ()
run_ = void
	. (flip (State.runN @nm) $ BitArray BitArray.empty)
	. (flip (State.runN @nm) $ RequestBuffer 100)

type States nm = '[State.Named nm Request, State.Named nm BitArray]

onDemand :: forall es r . forall nm -> (
	U.Member Pipe.P es,
	Members nm es, U.Member (Except.E String) es ) =>
	Eff.E es LBS.ByteString (Either BitArray.B LBS.ByteString) r
onDemand nm = fix \go -> State.getN nm >>= \case
	RequestBits ln -> takeBits nm ln >>=
		maybe (Except.throw errne) ((>> go) . Pipe.yield)
	RequestBytes ln -> takeBytes nm ln >>=
		maybe (Except.throw errne) ((>> go) . Pipe.yield)
	RequestBuffer ln -> takeBuffer nm ln >>=
		maybe (Except.throw errne) ((>> go) . Pipe.yield)
	RequestString -> takeString nm >>=
		maybe (Except.throw errne) ((>> go) . Pipe.yield)
	RequestPushBack ba ->
		State.modifyN nm (BitArray . (ba `BitArray.append`) . unBitArray) >>
		Pipe.yield (Right "") >> go
	where
	errne :: String
	errne = "Not enough ByteString"

onDemandWithInitial :: forall es r . forall nm ->
	(U.Member Pipe.P es, Members nm es, U.Member (Except.E String) es) =>
	LBS.ByteString ->
	Eff.E es LBS.ByteString (Either BitArray.B LBS.ByteString) r
onDemandWithInitial nm ib = do
	State.putN nm . BitArray $ BitArray.fromByteString ib
	onDemand nm

type Members nm es = (
	U.Member (State.Named nm Request) es,
	U.Member (State.Named nm BitArray) es )

data Request
	= RequestBits Int64
	| RequestBytes Int64
	| RequestBuffer Int64
	| RequestString
	| RequestPushBack BitArray.B
	deriving Show

newtype BitArray = BitArray { unBitArray :: BitArray.B } deriving Show

takeBits :: forall es o . forall nm -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm BitArray) es ) =>
	Int64 -> Eff.E es LBS.ByteString o
		(Maybe (Either BitArray.B LBS.ByteString))
takeBits nm ln = State.getsN nm unBitArray >>= \ba -> case BitArray.splitAt ln ba of
	Nothing -> readMore nm >>= bool (pure Nothing) (takeBits nm ln)
	Just (t, d) -> Just (BitArray.toByteString t) <$ State.putN nm (BitArray d)

takeBytes :: forall nm -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm BitArray) es ) =>
	Int64 -> Eff.E es LBS.ByteString o
		(Maybe (Either BitArray.B LBS.ByteString))
takeBytes nm ln = State.getsN nm unBitArray >>= \ba -> case BitArray.byteBoundary ba of
	Left (t, d) -> Just (Left t) <$ State.putN nm (BitArray d)
	Right b -> case BitArray.toByteString b of
		Left _ -> error "bad"
		Right bs -> case LBS.splitAt' ln bs of
			Nothing -> readMore nm >>= bool (pure Nothing) (takeBytes nm ln)
			Just (t, d) -> Just (Right t)
				<$ State.putN nm (BitArray $ BitArray.fromByteString d)

takeBuffer :: forall nm -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm BitArray) es ) =>
	Int64 -> Eff.E es LBS.ByteString o
		(Maybe (Either BitArray.B LBS.ByteString))
takeBuffer nm ln = State.getsN nm unBitArray >>= \ba -> case BitArray.byteBoundary ba of
	Left (t, d) -> Just (Left t) <$ State.putN nm (BitArray d)
	Right b -> case BitArray.toByteString b of
		Left _ -> error "bad"
		Right bs -> case LBS.splitAt' ln bs of
			Nothing -> readMore nm >>= bool
				(bool	(Just (Right bs) <$ State.putN nm (BitArray BitArray.empty))
					(pure Nothing) (LBS.null bs))
				(takeBuffer nm ln)
			Just (t, d) -> Just (Right t)
				<$ State.putN nm (BitArray $ BitArray.fromByteString d)

takeString :: forall nm ->
	(U.Member Pipe.P es, U.Member (State.Named nm BitArray) es) =>
	Eff.E es LBS.ByteString o
		(Maybe (Either BitArray.B LBS.ByteString))
takeString nm = State.getsN nm unBitArray >>= \ba -> case BitArray.byteBoundary ba of
	Left (t, d) -> Just (Left t) <$ State.putN nm (BitArray d)
	Right b -> case BitArray.toByteString b of
		Left _ -> error "bad"
		Right bs -> case splitString bs of
			Nothing -> readMore nm >>= bool (pure Nothing) (takeString nm)
			Just (t, d) -> Just (Right t)
				<$ State.putN nm (BitArray $ BitArray.fromByteString d)

splitString :: LBS.ByteString -> Maybe (LBS.ByteString, LBS.ByteString)
splitString bs = case LBS.span (/= 0) bs of
	(_, "") -> Nothing
	(t, LBS.uncons -> Just (z, d)) -> Just (LBS.snoc t z, d)
	_ -> error "Never occur"

readMore :: forall nm -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm BitArray) es ) =>
	Eff.E es LBS.ByteString o Bool
readMore nm = Pipe.awaitMaybe >>= \case
	Nothing -> pure False
	Just bs -> True <$ State.modifyN nm
		(BitArray . (`BitArray.appendByteString` bs) . unBitArray)
