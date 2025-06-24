{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.ByteString.FingerTree.OnDemand (

	-- * RUN

	run, States,

	-- * ON DEMAND

	onDemand, onDemandWithInitial, Members, Request(..), ByteString

	) where

import Control.Monad.Fix
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.HigherOpenUnion qualified as U
import Data.TypeLevel.List
import Data.HigherFunctor qualified as HFunctor
import Data.Bool
import Data.ByteString.FingerTree qualified as BSF

run :: forall nm es i o a . HFunctor.Loose (U.U es) =>
	Eff.E (States nm `Append` es) i o a ->
	Eff.E es i o ((a, Request), ByteString)
run = (flip (State.runN @nm) $ ByteString BSF.Empty)
	. (flip (State.runN @nm) $ RequestBuffer 100)

type States nm = '[State.Named nm Request, State.Named nm ByteString]

onDemand :: forall nm -> (
	U.Member Pipe.P es,
	Members nm es, U.Member (Except.E String) es ) =>
	Eff.E es BSF.ByteString BSF.ByteString r
onDemand nm = fix \go -> State.getN nm >>= \case
	RequestBytes ln -> takeBytes nm ln
		>>= maybe (Except.throw errne) ((>> go) . Pipe.yield)
	RequestBuffer ln -> takeBuffer nm ln
		>>= maybe (Except.throw errne) ((>> go) . Pipe.yield)
	RequestString -> takeString nm
		>>= maybe (Except.throw errne) ((>> go) . Pipe.yield)
	RequestPushBack bs ->
		State.modifyN nm (ByteString . (bs `BSF.append`) . unByteString) >>
		Pipe.yield "" >> go
	where
	errne :: String
	errne = "Not enough ByteString"

onDemandWithInitial :: forall nm -> (
	U.Member Pipe.P es,
	Members nm es, U.Member (Except.E String) es ) =>
	BSF.ByteString -> Eff.E es BSF.ByteString BSF.ByteString r
onDemandWithInitial nm ib = do
	State.putN nm $ ByteString ib
	onDemand nm

type Members nm es = (
	U.Member (State.Named nm Request) es,
	U.Member (State.Named nm ByteString) es )

data Request
	= RequestBytes Int
	| RequestBuffer Int
	| RequestString
	| RequestPushBack BSF.ByteString
	deriving Show

newtype ByteString = ByteString { unByteString :: BSF.ByteString } deriving Show

takeBytes :: forall nm -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm ByteString) es ) =>
	Int -> Eff.E es BSF.ByteString o (Maybe BSF.ByteString)
takeBytes nm ln = State.getsN nm unByteString >>= \bs ->
	case BSF.splitAt' ln bs of
		Nothing -> readMore nm >>= bool (pure Nothing) (takeBytes nm ln)
		Just (t, d) -> Just t <$ State.putN nm (ByteString d)

takeBuffer :: forall nm -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm ByteString) es ) =>
	Int -> Eff.E es BSF.ByteString o (Maybe BSF.ByteString)
takeBuffer nm ln = State.getsN nm unByteString >>= \bs ->
	case BSF.splitAt' ln bs of
		Nothing -> readMore nm >>= bool
			(bool	(Just bs
					<$ State.putN nm (ByteString BSF.Empty))
				(pure Nothing) (BSF.null bs))
			(takeBuffer nm ln)
		Just (t, d) -> Just t <$ State.putN nm (ByteString d)

takeString ::
	forall nm ->
	(U.Member Pipe.P es, U.Member (State.Named nm ByteString) es) =>
	Eff.E es BSF.ByteString o (Maybe BSF.ByteString)
takeString nm = State.getsN nm unByteString >>= \bs ->
	case splitString bs of
		Nothing -> readMore nm >>= bool (pure Nothing) (takeString nm)
		Just (t, d) -> Just t <$ State.putN nm (ByteString d)

splitString :: BSF.ByteString -> Maybe (BSF.ByteString, BSF.ByteString)
splitString bs = case BSF.span (/= 0) bs of
	(_, "") -> Nothing
	(t, BSF.uncons -> Just (z, d)) -> Just (BSF.snoc t z, d)
	_ -> error "Never occur"

readMore :: forall nm -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm ByteString) es ) =>
	Eff.E es BSF.ByteString o Bool
readMore nm = Pipe.awaitMaybe >>= \case
	Nothing -> pure False
	Just bs -> True <$ State.modifyN nm
		(ByteString . (`BSF.append` bs) . unByteString)
