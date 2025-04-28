{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase, BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Yaftee.Pipe.ByteString.OnDemand where

import Control.Monad.Fix
import Data.Bool
import Data.ByteString qualified as BS
import Data.BitArray qualified as BitArray

import Yaftee.Eff qualified as Eff
import Yaftee.Pipe qualified as Pipe
import Yaftee.State qualified as State
import Yaftee.Except qualified as Except
import Yaftee.OpenUnion qualified as Union

data Request
	= RequestBits Int
	| RequestBytes Int
	| RequestBuffer Int
	| RequestString
	| RequestPushBack BitArray.B
	deriving Show

onDemand :: (
	Union.Member Pipe.P effs,
	Union.Member (State.S Request) effs,
	Union.Member (State.S BitArray.B) effs,
	Union.Member (Except.E String) effs ) =>
	Eff.E effs (Maybe BS.ByteString) (Either BitArray.B BS.ByteString) r
onDemand = fix \go -> State.get >>= \case
	RequestBits ln -> takeBits ln >>=
		maybe (Except.throw errne) ((>> go) . Pipe.yield)
	RequestBytes ln -> takeBytes ln >>=
		maybe (Except.throw errne) ((>> go) . Pipe.yield)
	where
	errne :: String
	errne = "Not enough ByteString"

takeBits :: (
	Union.Member Pipe.P effs,
	Union.Member (State.S BitArray.B) effs ) =>
	Int -> Eff.E effs (Maybe BS.ByteString) o
		(Maybe (Either BitArray.B BS.ByteString))
takeBits ln = State.get >>= \ba -> case BitArray.splitAt ln ba of
	Nothing -> readMore >>= bool (pure Nothing) (takeBits ln)
	Just (t, d) -> Just (BitArray.toByteString t) <$ State.put d

takeBytes :: (
	Union.Member Pipe.P effs,
	Union.Member (State.S BitArray.B) effs ) =>
	Int -> Eff.E effs (Maybe BS.ByteString) o
		(Maybe (Either BitArray.B BS.ByteString))
takeBytes ln = State.get >>= \ba -> case BitArray.byteBoundary ba of
	Left (t, d) -> Just (Left t) <$ State.put (BitArray.fromByteString d)
	Right bs -> case splitAt' ln bs of
		Nothing -> readMore >>= bool (pure Nothing) (takeBytes ln)
		Just (t, d) ->
			Just (Right t) <$ State.put (BitArray.fromByteString d)

splitAt' :: Int -> BS.ByteString -> Maybe (BS.ByteString, BS.ByteString)
splitAt' ln bs = if BS.length bs < ln then Nothing else Just $ BS.splitAt ln bs

readMore :: (
	Union.Member Pipe.P effs,
	Union.Member (State.S BitArray.B) effs ) =>
	Eff.E effs (Maybe BS.ByteString) o Bool
readMore = Pipe.await >>= \case
	Nothing -> pure False
	Just bs -> True <$ State.modify (`BitArray.appendByteString` bs)
