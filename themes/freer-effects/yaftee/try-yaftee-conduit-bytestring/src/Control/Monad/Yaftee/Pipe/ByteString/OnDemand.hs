{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.ByteString.OnDemand (

	-- * RUN

	run_, States,

	-- * ON DEMAND

	onDemand, Members, Request(..), BitArray

	) where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.HigherOpenUnion qualified as Union
import Data.TypeLevel.List
import Data.HigherFunctor qualified as HFunctor
import Data.Bool
import Data.ByteString qualified as BS
import Data.ByteString.BitArray qualified as BitArray

run_ :: HFunctor.Loose (Union.U es) =>
	Eff.E (States `Append` es) i o a -> Eff.E es i o ()
run_ = void . (`State.run` RequestBuffer 100) . (`State.run` BitArray BitArray.empty)

type States = '[State.S BitArray, State.S Request]

onDemand :: (
	Union.Member Pipe.P es,
	Members es, Union.Member (Except.E String) es ) =>
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
	RequestPushBack ba ->
		State.modify (BitArray . (ba `BitArray.append`) . unBitArray) >>
		Pipe.yield (Right "") >> go
	where
	errne :: String
	errne = "Not enough ByteString"

type Members es = (
	Union.Member (State.S Request) es,
	Union.Member (State.S BitArray) es )

data Request
	= RequestBits Int
	| RequestBytes Int
	| RequestBuffer Int
	| RequestString
	| RequestPushBack BitArray.B
	deriving Show

newtype BitArray = BitArray { unBitArray :: BitArray.B } deriving Show

takeBits :: (
	Union.Member Pipe.P es,
	Union.Member (State.S BitArray) es ) =>
	Int -> Eff.E es BS.ByteString o
		(Maybe (Either BitArray.B BS.ByteString))
takeBits ln = State.gets unBitArray >>= \ba -> case BitArray.splitAt ln ba of
	Nothing -> readMore >>= bool (pure Nothing) (takeBits ln)
	Just (t, d) -> Just (BitArray.toByteString t) <$ State.put (BitArray d)

takeBytes :: (
	Union.Member Pipe.P es,
	Union.Member (State.S BitArray) es ) =>
	Int -> Eff.E es BS.ByteString o
		(Maybe (Either BitArray.B BS.ByteString))
takeBytes ln = State.gets unBitArray >>= \ba -> case BitArray.byteBoundary ba of
	Left (t, d) -> Just (Left t) <$ State.put (BitArray d)
	Right b -> case BitArray.toByteString b of
		Left _ -> error "bad"
		Right bs -> case splitAt' ln bs of
			Nothing -> readMore >>= bool (pure Nothing) (takeBytes ln)
			Just (t, d) -> Just (Right t)
				<$ State.put (BitArray $ BitArray.fromByteString d)

takeBuffer :: (
	Union.Member Pipe.P effs,
	Union.Member (State.S BitArray) effs ) =>
	Int -> Eff.E effs BS.ByteString o
		(Maybe (Either BitArray.B BS.ByteString))
takeBuffer ln = State.gets unBitArray >>= \ba -> case BitArray.byteBoundary ba of
	Left (t, d) -> Just (Left t) <$ State.put (BitArray d)
	Right b -> case BitArray.toByteString b of
		Left _ -> error "bad"
		Right bs -> case splitAt' ln bs of
			Nothing -> readMore >>= bool
				(bool	(Just (Right bs) <$ State.put (BitArray BitArray.empty))
					(pure Nothing) (BS.null bs))
				(takeBuffer ln)
			Just (t, d) -> Just (Right t)
				<$ State.put (BitArray $ BitArray.fromByteString d)

takeString ::
	(Union.Member Pipe.P effs, Union.Member (State.S BitArray) effs) =>
	Eff.E effs BS.ByteString o
		(Maybe (Either BitArray.B BS.ByteString))
takeString = State.gets unBitArray >>= \ba -> case BitArray.byteBoundary ba of
	Left (t, d) -> Just (Left t) <$ State.put (BitArray d)
	Right b -> case BitArray.toByteString b of
		Left _ -> error "bad"
		Right bs -> case splitString bs of
			Nothing -> readMore >>= bool (pure Nothing) takeString
			Just (t, d) -> Just (Right t)
				<$ State.put (BitArray $ BitArray.fromByteString d)

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
	Union.Member (State.S BitArray) es
	) =>
	Eff.E es BS.ByteString o Bool
readMore = Pipe.awaitMaybe >>= \case
	Nothing -> pure False
	Just bs -> True <$ State.modify
		(BitArray . (`BitArray.appendByteString` bs) . unBitArray)
