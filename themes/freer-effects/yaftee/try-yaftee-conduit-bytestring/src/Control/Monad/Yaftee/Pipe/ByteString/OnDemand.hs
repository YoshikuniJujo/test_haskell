{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.ByteString.OnDemand (

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
import Control.HigherOpenUnion qualified as Union
import Data.TypeLevel.List
import Data.HigherFunctor qualified as HFunctor
import Data.Bool
import Data.ByteString qualified as BS
import Data.ByteString.BitArray qualified as BitArray

run_ :: forall nm es i o a . HFunctor.Loose (Union.U es) =>
	Eff.E (States nm `Append` es) i o a -> Eff.E es i o ()
run_ = void . (flip (State.runN @nm) $ RequestBuffer 100)
	. (flip (State.runN @nm) $ BitArray BitArray.empty)

type States nm = '[State.Named nm BitArray, State.Named nm Request]

onDemand :: forall es r . forall nm -> (
	Union.Member Pipe.P es,
	Members nm es, Union.Member (Except.E String) es ) =>
	Eff.E es BS.ByteString (Either BitArray.B BS.ByteString) r
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

onDemandWithInitial :: forall es r . forall nm -> (
	Union.Member Pipe.P es,
	Members nm es, Union.Member (Except.E String) es ) =>
	BS.ByteString ->
	Eff.E es BS.ByteString (Either BitArray.B BS.ByteString) r
onDemandWithInitial nm ib = do
	State.putN nm . BitArray $ BitArray.fromByteString ib
	onDemand nm

type Members nm es = (
	Union.Member (State.Named nm Request) es,
	Union.Member (State.Named nm BitArray) es )

data Request
	= RequestBits Int
	| RequestBytes Int
	| RequestBuffer Int
	| RequestString
	| RequestPushBack BitArray.B
	deriving Show

newtype BitArray = BitArray { unBitArray :: BitArray.B } deriving Show

takeBits :: forall es o . forall nm -> (
	Union.Member Pipe.P es,
	Union.Member (State.Named nm BitArray) es ) =>
	Int -> Eff.E es BS.ByteString o
		(Maybe (Either BitArray.B BS.ByteString))
takeBits nm ln = State.getsN nm unBitArray >>= \ba -> case BitArray.splitAt ln ba of
	Nothing -> readMore nm >>= bool (pure Nothing) (takeBits nm ln)
	Just (t, d) -> Just (BitArray.toByteString t) <$ State.putN nm (BitArray d)

takeBytes :: forall nm -> (
	Union.Member Pipe.P es,
	Union.Member (State.Named nm BitArray) es ) =>
	Int -> Eff.E es BS.ByteString o
		(Maybe (Either BitArray.B BS.ByteString))
takeBytes nm ln = State.getsN nm unBitArray >>= \ba -> case BitArray.byteBoundary ba of
	Left (t, d) -> Just (Left t) <$ State.putN nm (BitArray d)
	Right b -> case BitArray.toByteString b of
		Left _ -> error "bad"
		Right bs -> case splitAt' ln bs of
			Nothing -> readMore nm >>= bool (pure Nothing) (takeBytes nm ln)
			Just (t, d) -> Just (Right t)
				<$ State.putN nm (BitArray $ BitArray.fromByteString d)

takeBuffer :: forall nm -> (
	Union.Member Pipe.P effs,
	Union.Member (State.Named nm BitArray) effs ) =>
	Int -> Eff.E effs BS.ByteString o
		(Maybe (Either BitArray.B BS.ByteString))
takeBuffer nm ln = State.getsN nm unBitArray >>= \ba -> case BitArray.byteBoundary ba of
	Left (t, d) -> Just (Left t) <$ State.putN nm (BitArray d)
	Right b -> case BitArray.toByteString b of
		Left _ -> error "bad"
		Right bs -> case splitAt' ln bs of
			Nothing -> readMore nm >>= bool
				(bool	(Just (Right bs) <$ State.putN nm (BitArray BitArray.empty))
					(pure Nothing) (BS.null bs))
				(takeBuffer nm ln)
			Just (t, d) -> Just (Right t)
				<$ State.putN nm (BitArray $ BitArray.fromByteString d)

takeString ::
	forall nm ->
	(Union.Member Pipe.P effs, Union.Member (State.Named nm BitArray) effs) =>
	Eff.E effs BS.ByteString o
		(Maybe (Either BitArray.B BS.ByteString))
takeString nm = State.getsN nm unBitArray >>= \ba -> case BitArray.byteBoundary ba of
	Left (t, d) -> Just (Left t) <$ State.putN nm (BitArray d)
	Right b -> case BitArray.toByteString b of
		Left _ -> error "bad"
		Right bs -> case splitString bs of
			Nothing -> readMore nm >>= bool (pure Nothing) (takeString nm)
			Just (t, d) -> Just (Right t)
				<$ State.putN nm (BitArray $ BitArray.fromByteString d)

	{- case splitString . fromRight $ BitArray.toByteString bs of
		Nothing -> readMore nm >>= bool (pure Nothing) takeString
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

readMore :: forall nm -> (
	Union.Member Pipe.P es,
	Union.Member (State.Named nm BitArray) es
	) =>
	Eff.E es BS.ByteString o Bool
readMore nm = Pipe.awaitMaybe >>= \case
	Nothing -> pure False
	Just bs -> True <$ State.modifyN nm
		(BitArray . (`BitArray.appendByteString` bs) . unBitArray)
