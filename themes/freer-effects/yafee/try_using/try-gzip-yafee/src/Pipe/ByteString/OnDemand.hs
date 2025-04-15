{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Pipe.ByteString.OnDemand where

import Prelude hiding (splitAt)
import Control.Monad.Yafee.Eff qualified as Eff
import Control.Monad.Yafee.Pipe qualified as Pipe
import Control.Monad.Yafee.State qualified as State
import Control.Monad.Yafee.Except qualified as Except
import Control.OpenUnion qualified as Union
import Data.Bool
import Data.ByteString qualified as BS

import BitArray qualified as BitArray

data Request
	= RequestBytes Int
	| RequestString
	| RequestBuffer Int
	| RequestBits Int
	| RequestPushBack BitArray.B
	deriving Show

onDemand :: (
	Union.Member (State.S Request) effs,
	Union.Member (State.S BitArray.B) effs,
	Union.Member (Except.E String) effs ) =>
	Eff.E (Pipe.P BS.ByteString (Either BitArray.B BS.ByteString) ': effs) ()
onDemand = State.get >>= \case
	RequestBytes ln -> do
		mt <- takeBytes' ln
		maybe (Except.throw @String "Not enough ByteString")
			(\t -> Pipe.yield t >> onDemand) mt
	RequestString -> do
		mt <- takeString'
		maybe (Except.throw @String "Not enough ByteString")
			(\t -> Pipe.yield t >> onDemand) mt
	RequestBuffer ln -> do
		mt <- takeBuffer' ln
		maybe (Except.throw @String "End of input")
			(\t -> Pipe.yield t >> onDemand) mt
	RequestBits ln -> do
		mt <- takeBits ln
		maybe (Except.throw @String "Not enough ByteString")
			(\t -> Pipe.yield t >> onDemand) mt
	RequestPushBack ba -> do
		State.modify (ba `BitArray.append`)
		Pipe.yield $ Right ""
		onDemand

takeBytes :: (
	Union.Member (State.S BS.ByteString) effs ) =>
	Int -> Eff.E (Pipe.P BS.ByteString o ': effs) (Maybe BS.ByteString)
takeBytes ln = State.get >>= \bs ->
	if BS.length bs < ln
	then readMore >>= bool (pure Nothing) (takeBytes ln)
	else let (t, d) = BS.splitAt ln bs in Just t <$ State.put d

takeBytes' :: (
	Union.Member (State.S BitArray.B) effs ) =>
	Int -> Eff.E (Pipe.P BS.ByteString o ': effs) (Maybe (Either BitArray.B BS.ByteString))
takeBytes' ln = State.get >>= \ba ->
	case BitArray.byteBoundary ba of
		Left (t, d) -> Just (Left t) <$ State.put (BitArray.fromByteString d)
		Right bs -> if BS.length bs < ln
			then readMore' >>= bool (pure Nothing) (takeBytes' ln)
			else let (t, d) = BS.splitAt ln bs in Just (Right t)
				<$ State.put (BitArray.fromByteString d)

takeString :: (
	Union.Member (State.S BS.ByteString) effs ) =>
	Eff.E (Pipe.P BS.ByteString o ': effs) (Maybe BS.ByteString)
takeString = State.get >>= \bs -> case splitString bs of
	Nothing -> readMore >>= bool (pure Nothing) takeString
	Just (t, d) -> Just t <$ State.put d

takeString' :: (
	Union.Member (State.S BitArray.B) effs ) =>
	Eff.E (Pipe.P BS.ByteString o ': effs) (Maybe (Either BitArray.B BS.ByteString))
takeString' = State.get >>= \ba -> case BitArray.byteBoundary ba of
	Left (t, d) -> Just (Left t) <$ State.put (BitArray.fromByteString d)
	Right bs -> case splitString bs of
		Nothing -> readMore' >>= bool (pure Nothing) takeString'
		Just (t, d) -> Just (Right t)
			<$ State.put (BitArray.fromByteString d)

takeBuffer :: (
	Union.Member (State.S BS.ByteString) effs ) =>
	Int -> Eff.E (Pipe.P BS.ByteString o ': effs) (Maybe BS.ByteString)
takeBuffer ln = State.get >>= \bs ->
	if BS.length bs < ln
	then do	b <- readMore
		if b then takeBuffer ln else if BS.null bs then pure Nothing else Just bs <$ State.put ("" :: BS.ByteString)
	else let (t, d) = BS.splitAt ln bs in Just t <$ State.put d

takeBuffer' :: (
	Union.Member (State.S BitArray.B) effs ) =>
	Int -> Eff.E (Pipe.P BS.ByteString o ': effs) (Maybe (Either BitArray.B BS.ByteString))
takeBuffer' ln = State.get >>= \ba -> case BitArray.byteBoundary ba of
	Left (t, d) -> Just (Left t) <$ State.put (BitArray.fromByteString d)
	Right bs -> if BS.length bs < ln
		then do	b <- readMore'
			if b
			then takeBuffer' ln
			else if BS.null bs
			then pure Nothing
			else Just (Right bs) <$ State.put BitArray.empty
		else let (t, d) = BS.splitAt ln bs in Just (Right t) <$ State.put (BitArray.fromByteString d)

takeBits :: Union.Member (State.S BitArray.B) effs =>
	Int -> Eff.E (Pipe.P BS.ByteString o ': effs) (Maybe (Either BitArray.B BS.ByteString))
takeBits ln = State.get >>= \ba -> case BitArray.splitAt ln ba of
	Nothing -> readMore' >>= bool (takeBits ln) (pure Nothing)
	Just (t, d) -> Just (BitArray.toByteString t) <$ State.put d

splitString :: BS.ByteString -> Maybe (BS.ByteString, BS.ByteString)
splitString bs = case BS.span (/= 0) bs of
	(_, "") -> Nothing
	(t, BS.uncons -> Just (z, d)) -> Just (BS.snoc t z, d)
	_ -> error "Never occur"

readMore :: (
	Union.Member (State.S BS.ByteString) effs
	) =>
	Eff.E (Pipe.P BS.ByteString o ': effs) Bool
readMore = Pipe.await >>= \case
	Nothing -> pure False
	Just bs -> True <$ State.modify (`BS.append` bs)

readMore' :: forall o effs . (
	Union.Member (State.S BitArray.B) effs ) =>
	Eff.E (Pipe.P BS.ByteString o ': effs) Bool
readMore' = Pipe.await' o >>= \case
	Nothing -> pure False
	Just bs -> True <$ State.modify (`BitArray.appendByteString` bs)
