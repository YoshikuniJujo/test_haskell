{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Pipe.ByteString.OnDemand where

import Control.Monad.Yafee.Eff qualified as Eff
import Control.Monad.Yafee.Pipe qualified as Pipe
import Control.Monad.Yafee.State qualified as State
import Control.Monad.Yafee.Except qualified as Except
import Control.OpenUnion qualified as Union
import Data.Bool
import Data.ByteString qualified as BS

data Request
	= RequestBytes Int
	| RequestString
	| RequestBuffer Int
	deriving Show

onDemand :: (
	Union.Member (Pipe.P BS.ByteString BS.ByteString) effs,
	Union.Member (State.S Request) effs,
	Union.Member (State.S BS.ByteString) effs,
	Union.Member (Except.E String) effs ) =>
	Eff.E effs ()
onDemand = State.get >>= \case
	RequestBytes ln -> do
		mt <- takeBytes BS.ByteString ln
		maybe (Except.throw @String "Not enough ByteString")
			(\t -> Pipe.yield BS.ByteString t >> onDemand) mt
	RequestString -> do
		mt <- takeString BS.ByteString
		maybe (Except.throw @String "Not enough ByteString")
			(\t -> Pipe.yield BS.ByteString t >> onDemand) mt
	RequestBuffer ln -> do
		mt <- takeBuffer BS.ByteString ln
		maybe (Except.throw @String "End of input")
			(\t -> Pipe.yield BS.ByteString t >> onDemand) mt

takeBytes :: forall o -> (
	Union.Member (Pipe.P BS.ByteString o) effs,
	Union.Member (State.S BS.ByteString) effs ) =>
	Int -> Eff.E effs (Maybe BS.ByteString)
takeBytes o ln = State.get >>= \bs ->
	if BS.length bs < ln
	then readMore o >>= bool (pure Nothing) (takeBytes o ln)
	else let (t, d) = BS.splitAt ln bs in Just t <$ State.put d

takeString :: forall o -> (
	Union.Member (Pipe.P BS.ByteString o) effs,
	Union.Member (State.S BS.ByteString) effs ) =>
	Eff.E effs (Maybe BS.ByteString)
takeString o = State.get >>= \bs -> case splitString bs of
	Nothing -> readMore o >>= bool (pure Nothing) (takeString o)
	Just (t, d) -> Just t <$ State.put d

takeBuffer :: forall o -> (
	Union.Member (Pipe.P BS.ByteString o) effs,
	Union.Member (State.S BS.ByteString) effs ) =>
	Int -> Eff.E effs (Maybe BS.ByteString)
takeBuffer o ln = State.get >>= \bs ->
	if BS.length bs < ln
	then do	b <- readMore o
		if b then takeBuffer o ln else if BS.null bs then pure Nothing else pure $ Just bs
	else let (t, d) = BS.splitAt ln bs in Just t <$ State.put d

splitString :: BS.ByteString -> Maybe (BS.ByteString, BS.ByteString)
splitString bs = case BS.span (/= 0) bs of
	(_, "") -> Nothing
	(t, BS.uncons -> Just (z, d)) -> Just (BS.snoc t z, d)

readMore :: forall o -> (
	Union.Member (Pipe.P BS.ByteString o) effs,
	Union.Member (State.S BS.ByteString) effs
	) =>
	Eff.E effs Bool
readMore o = Pipe.await o >>= \case
	Nothing -> pure False
	Just bs -> True <$ State.modify (`BS.append` bs)
