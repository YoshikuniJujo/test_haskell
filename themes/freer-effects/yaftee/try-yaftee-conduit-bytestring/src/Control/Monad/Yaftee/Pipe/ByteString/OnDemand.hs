{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.ByteString.OnDemand where

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
	| RequestPushBadk BitArray.B
	deriving Show

onDemand :: (
	Union.Member Pipe.P es,
	Union.Member (State.S Request) es,
	Union.Member (State.S BitArray.B) es,
	Union.Member (Except.E String) es
	) =>
	Eff.E es (Maybe BS.ByteString) (Either BitArray.B BS.ByteString) r
onDemand = fix \go -> State.get >>= \case
	RequestBits ln -> takeBits ln >>=
		maybe (Except.throw errne) ((>> go) . Pipe.yield)
	RequestBytes ln -> takeBytes ln >>=
		maybe (Except.throw errne) ((>> go) . Pipe.yield)
	where
	errne :: String
	errne = "Not enough ByteString"

takeBits :: (
	Union.Member Pipe.P es,
	Union.Member (State.S BitArray.B) es ) =>
	Int -> Eff.E es (Maybe BS.ByteString) o
		(Maybe (Either BitArray.B BS.ByteString))
takeBits ln = State.get >>= \ba -> case BitArray.splitAt ln ba of
	Nothing -> readMore >>= bool (pure Nothing) (takeBits ln)
	Just (t, d) -> Just (BitArray.toByteString t) <$ State.put d

takeBytes :: (
	Union.Member Pipe.P es,
	Union.Member (State.S BitArray.B) es ) =>
	Int -> Eff.E es (Maybe BS.ByteString) o
		(Maybe (Either BitArray.B BS.ByteString))
takeBytes ln = State.get >>= \ba -> case BitArray.byteBoundary ba of
	Left (t, d) -> Just (Left t) <$ State.put d
	Right b -> case BitArray.toByteString b of
		Left _ -> error "bad"
		Right bs -> case splitAt' ln bs of
			Nothing -> readMore >>= bool (pure Nothing) (takeBytes ln)
			Just (t, d) ->
				Just (Right t) <$ State.put (BitArray.fromByteString d)

-- takeBuffer ::

splitAt' :: Int -> BS.ByteString -> Maybe (BS.ByteString, BS.ByteString)
splitAt' ln bs = if BS.length bs < ln then Nothing else Just $ BS.splitAt ln bs

readMore :: (
	Union.Member Pipe.P es,
	Union.Member (State.S BitArray.B) es
	) =>
	Eff.E es (Maybe BS.ByteString) o Bool
readMore = Pipe.await >>= \case
	Nothing -> pure False
	Just bs -> True <$ State.modify (`BitArray.appendByteString` bs)
