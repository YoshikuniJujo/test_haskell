{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Pipe.ByteString.OnDemand where

import Control.Monad.Fix
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.HigherOpenUnion qualified as Union
import Data.Bool
import Data.ByteString qualified as BS
import Data.BitArray qualified as BitArray

data Request
	= RequestBits Int
	| RequestBytes Int
	| RequestBuffer Int
	| RequestString
	| RequestPushBack BitArray.B
	deriving Show

onDemand :: (
	Union.Member (State.S Request) effs,
	Union.Member (State.S BitArray.B) effs,
	Union.Member (Except.E String) effs ) =>
	Eff.E (Pipe.P
		(Maybe BS.ByteString)
		(Either BitArray.B BS.ByteString) ': effs) ()
onDemand = fix \go -> State.get >>= \case
	RequestBits ln -> takeBits ln >>=
		maybe (Except.throw errne) (\t -> Pipe.yield t >> go)
	RequestBytes ln -> takeBytes ln >>=
		maybe (Except.throw errne) (\t -> Pipe.yield t >> go)
	RequestBuffer ln -> takeBuffer ln >>=
		maybe (Except.throw errne) (\t -> Pipe.yield t >> go)
	RequestString -> takeString >>=
		maybe (Except.throw errne) (\t -> Pipe.yield t >> go)
	RequestPushBack ba -> State.modify (ba `BitArray.append`) >>
		Pipe.yield (Right "") >> go
	where
	errne :: String
	errne = "Not enough ByteString"

takeBits :: Union.Member (State.S BitArray.B) effs => Int ->
	Eff.E (Pipe.P (Maybe BS.ByteString) o ': effs)
		(Maybe (Either BitArray.B BS.ByteString))
takeBits ln = State.get >>= \ba -> case BitArray.splitAt ln ba of
	Nothing -> readMore >>= bool (pure Nothing) (takeBits ln)
	Just (t, d) -> Just (BitArray.toByteString t) <$ State.put d

takeBytes :: Union.Member (State.S BitArray.B) effs => Int ->
	Eff.E (Pipe.P (Maybe BS.ByteString) o ': effs)
		(Maybe (Either BitArray.B BS.ByteString))
takeBytes ln = State.get >>= \ba -> case BitArray.byteBoundary ba of
	Left (t, d) -> Just (Left t) <$ State.put (BitArray.fromByteString d)
	Right bs -> case splitAt' ln bs of
		Nothing -> readMore >>= bool (pure Nothing) (takeBytes ln)
		Just (t, d) ->
			Just (Right t) <$ State.put (BitArray.fromByteString d)

splitAt' :: Int -> BS.ByteString -> Maybe (BS.ByteString, BS.ByteString)
splitAt' ln bs = if BS.length bs < ln then Nothing else Just $ BS.splitAt ln bs

takeBuffer :: Union.Member (State.S BitArray.B) effs =>
	Int -> Eff.E (Pipe.P (Maybe BS.ByteString) o ': effs)
		(Maybe (Either BitArray.B BS.ByteString))
takeBuffer ln = State.get >>= \ba -> case BitArray.byteBoundary ba of
	Left (t, d) -> Just (Left t) <$ State.put (BitArray.fromByteString d)
	Right bs -> case splitAt' ln bs of
		Nothing -> readMore >>= bool
			(bool	(Just (Right bs) <$ State.put BitArray.empty)
				(pure Nothing) (BS.null bs))
			(takeBuffer ln)
		Just (t, d) -> Just (Right t)
			<$ State.put (BitArray.fromByteString d)

takeString :: Union.Member (State.S BitArray.B) effs =>
	Eff.E (Pipe.P (Maybe BS.ByteString) o ': effs)
		(Maybe (Either BitArray.B BS.ByteString))
takeString = State.get >>= \ba -> case BitArray.byteBoundary ba of
	Left (t, d) -> Just (Left t) <$ State.put (BitArray.fromByteString d)
	Right bs -> case splitString bs of
		Nothing -> readMore >>= bool (pure Nothing) takeString
		Just (t, d) -> Just (Right t)
			<$ State.put (BitArray.fromByteString d)

readMore :: (Union.Member (State.S BitArray.B) effs) =>
	Eff.E (Pipe.P (Maybe BS.ByteString) o ': effs) Bool
readMore = Pipe.await >>= \case
	Nothing -> pure False
	Just bs -> True <$ State.modify (`BitArray.appendByteString` bs)

splitString :: BS.ByteString -> Maybe (BS.ByteString, BS.ByteString)
splitString bs = case BS.span (/= 0) bs of
	(_, "") -> Nothing
	(t, BS.uncons -> Just (z, d)) -> Just (BS.snoc t z, d)
	_ -> error "Never occur"
