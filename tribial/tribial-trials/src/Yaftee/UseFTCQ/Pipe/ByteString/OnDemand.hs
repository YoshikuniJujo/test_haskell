{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase, BlockArguments, OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Yaftee.UseFTCQ.Pipe.ByteString.OnDemand where

import Control.Monad.Fix
import Data.Bool
import Data.ByteString qualified as BS
import Data.BitArrayNew qualified as BitArray
-- import Data.BitArray qualified as BitArray

import Yaftee.UseFTCQ.Eff qualified as Eff
import Yaftee.UseFTCQ.Pipe qualified as Pipe
import Yaftee.UseFTCQ.State qualified as State
import Yaftee.UseFTCQ.Except qualified as Except
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
	RequestBuffer ln -> takeBuffer ln >>=
		maybe (Except.throw errne) ((>> go) . Pipe.yield)
	RequestString -> takeString >>=
		maybe (Except.throw errne) ((>> go) . Pipe.yield)
	RequestPushBack ba -> State.modify (ba `BitArray.append`) >>
		Pipe.yield (Right "") >> go
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
	Left (t, d) -> Just (Left t) <$ State.put d
	Right bs -> case splitAt' ln (fromRight $ BitArray.toByteString bs) of
		Nothing -> readMore >>= bool (pure Nothing) (takeBytes ln)
		Just (t, d) ->
			Just (Right t) <$ State.put (BitArray.fromByteString d)

fromRight (Right x) = x
fromRight (Left y) = error $ "fromRight: error " ++ show y

takeBuffer :: (
	Union.Member Pipe.P effs,
	Union.Member (State.S BitArray.B) effs ) =>
	Int -> Eff.E effs (Maybe BS.ByteString) o
		(Maybe (Either BitArray.B BS.ByteString))
takeBuffer ln = State.get >>= \ba -> case BitArray.byteBoundary ba of
	Left (t, d) -> Just (Left t) <$ State.put d
	Right bs -> case splitAt' ln (fromRight $ BitArray.toByteString bs) of
		Nothing -> readMore >>= bool
			(bool	(Just (BitArray.toByteString bs) <$ State.put BitArray.empty)
				(pure Nothing) (BitArray.null bs))
			(takeBuffer ln)
		Just (t, d) -> Just (Right t)
			<$ State.put (BitArray.fromByteString d)

takeString :: (
	Union.Member Pipe.P effs,
	Union.Member (State.S BitArray.B) effs ) =>
	Eff.E effs (Maybe BS.ByteString) o
		(Maybe (Either BitArray.B BS.ByteString))
takeString = State.get >>= \ba -> case BitArray.byteBoundary ba of
	Left (t, d) -> Just (Left t) <$ State.put d
	Right bs -> case splitString . fromRight $ BitArray.toByteString bs of
		Nothing -> readMore >>= bool (pure Nothing) takeString
		Just (t, d) -> Just (Right t)
			<$ State.put (BitArray.fromByteString d)

splitString :: BS.ByteString -> Maybe (BS.ByteString, BS.ByteString)
splitString bs = case BS.span (/= 0) bs of
	(_, "") -> Nothing
	(t, BS.uncons -> Just (z, d)) -> Just (BS.snoc t z, d)
	_ -> error "Never occur"

splitAt' :: Int -> BS.ByteString -> Maybe (BS.ByteString, BS.ByteString)
splitAt' ln bs = if BS.length bs < ln then Nothing else Just $ BS.splitAt ln bs

readMore :: (
	Union.Member Pipe.P effs,
	Union.Member (State.S BitArray.B) effs ) =>
	Eff.E effs (Maybe BS.ByteString) o Bool
readMore = readMore'

readMoreFoo :: (
	Union.Member Pipe.P effs,
	Union.Member (State.S BitArray.B) effs ) =>
	Eff.E effs (Maybe BS.ByteString) o Bool
readMoreFoo = Pipe.await >>= \case
	Nothing -> pure False
	Just bs -> True <$ State.modify (`BitArray.appendByteString` bs)

readMore' :: (
	Union.Member Pipe.P effs,
	Union.Member (State.S BitArray.B) effs ) =>
	Eff.E effs (Maybe BS.ByteString) o Bool
readMore' = (Pipe.isMore >>=) . bool (pure False)
	$ Pipe.await >>= \case
		Nothing -> pure False
		Just bs -> True <$ State.modify (`BitArray.appendByteString` bs)
