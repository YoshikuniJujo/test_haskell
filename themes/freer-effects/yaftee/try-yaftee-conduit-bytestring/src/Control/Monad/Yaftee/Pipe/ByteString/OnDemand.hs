{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.ByteString.OnDemand (
	onDemand, Request(..)
	) where

import Control.Monad.Fix
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.HigherOpenUnion qualified as Union
import Data.Bool
import Data.ByteString qualified as BS
import Data.ByteString.BitArray qualified as BitArray

data Request
	= RequestBits Int
	| RequestBytes Int
	| RequestBuffer Int
	| RequestString
	| RequestPushBack BitArray.B
	deriving Show

onDemand :: (
	Union.Member Pipe.P es,
	Union.Member (State.S Request) es,
	Union.Member (State.S BitArray.B) es,
	Union.Member (Except.E String) es
	) =>
	Eff.E es BS.ByteString (Either BitArray.B BS.ByteString) r
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
	Union.Member Pipe.P es,
	Union.Member (State.S BitArray.B) es ) =>
	Int -> Eff.E es BS.ByteString o
		(Maybe (Either BitArray.B BS.ByteString))
takeBits ln = State.get >>= \ba -> case BitArray.splitAt ln ba of
	Nothing -> readMore >>= bool (pure Nothing) (takeBits ln)
	Just (t, d) -> Just (BitArray.toByteString t) <$ State.put d

takeBytes :: (
	Union.Member Pipe.P es,
	Union.Member (State.S BitArray.B) es ) =>
	Int -> Eff.E es BS.ByteString o
		(Maybe (Either BitArray.B BS.ByteString))
takeBytes ln = State.get >>= \ba -> case BitArray.byteBoundary ba of
	Left (t, d) -> Just (Left t) <$ State.put d
	Right b -> case BitArray.toByteString b of
		Left _ -> error "bad"
		Right bs -> case splitAt' ln bs of
			Nothing -> readMore >>= bool (pure Nothing) (takeBytes ln)
			Just (t, d) ->
				Just (Right t) <$ State.put (BitArray.fromByteString d)

takeBuffer :: (
	Union.Member Pipe.P effs,
	Union.Member (State.S BitArray.B) effs ) =>
	Int -> Eff.E effs BS.ByteString o
		(Maybe (Either BitArray.B BS.ByteString))
takeBuffer ln = State.get >>= \ba -> case BitArray.byteBoundary ba of
	Left (t, d) -> Just (Left t) <$ State.put d
	Right b -> case BitArray.toByteString b of
		Left _ -> error "bad"
		Right bs -> case splitAt' ln bs of
			Nothing -> readMore >>= bool
				(bool	(Just (Right bs) <$ State.put BitArray.empty)
					(pure Nothing) (BS.null bs))
				(takeBuffer ln)
			Just (t, d) -> Just (Right t)
				<$ State.put (BitArray.fromByteString d)

takeString ::
	(Union.Member Pipe.P effs, Union.Member (State.S BitArray.B) effs) =>
	Eff.E effs BS.ByteString o
		(Maybe (Either BitArray.B BS.ByteString))
takeString = State.get >>= \ba -> case BitArray.byteBoundary ba of
	Left (t, d) -> Just (Left t) <$ State.put d
	Right b -> case BitArray.toByteString b of
		Left _ -> error "bad"
		Right bs -> case splitString bs of
			Nothing -> readMore >>= bool (pure Nothing) takeString
			Just (t, d) -> Just (Right t)
				<$ State.put (BitArray.fromByteString d)

	{- case splitString . fromRight $ BitArray.toByteString bs of
		Nothing -> readMore >>= bool (pure Nothing) takeString
		Just (t, d) -> Just (Right t)
			<$ State.put (BitArray.fromByteString d)
			-}

splitString :: BS.ByteString -> Maybe (BS.ByteString, BS.ByteString)
splitString bs = case BS.span (/= 0) bs of
	(_, "") -> Nothing
	(t, BS.uncons -> Just (z, d)) -> Just (BS.snoc t z, d)
	_ -> error "Never occur"

splitAt' :: Int -> BS.ByteString -> Maybe (BS.ByteString, BS.ByteString)
splitAt' ln bs = if BS.length bs < ln then Nothing else Just $ BS.splitAt ln bs

readMore :: (
	Union.Member Pipe.P es,
	Union.Member (State.S BitArray.B) es
	) =>
	Eff.E es BS.ByteString o Bool
readMore = Pipe.awaitMaybe >>= \case
	Nothing -> pure False
	Just bs -> True <$ State.modify (`BitArray.appendByteString` bs)
