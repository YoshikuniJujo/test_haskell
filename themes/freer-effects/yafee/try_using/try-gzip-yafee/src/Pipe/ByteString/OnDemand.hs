{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
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
import Data.Bits
import Data.Bool
import Data.Word
import Data.ByteString qualified as BS

data Request
	= RequestBytes Int
	| RequestString
	| RequestBuffer Int
	| RequestBits Int
	deriving Show

data BitArray = BitArray {
	bit0 :: Int, bitsLen :: Int,
	bitsBody :: BS.ByteString } deriving Show

byteStringToBitArray :: BS.ByteString -> BitArray
byteStringToBitArray bs = BitArray {
	bit0 = 0, bitsLen = 8 * BS.length bs, bitsBody = bs }

appendBitArrayAndByteString :: BitArray -> BS.ByteString -> BitArray
appendBitArrayAndByteString
	BitArray { bit0 = i0, bitsLen = ln, bitsBody = bs } bs' =
	BitArray {
		bit0 = i0, bitsLen = ln + 8 * BS.length bs',
		bitsBody = bs `BS.append` bs' }

bitArrayToByteBoundary :: BitArray -> Either (BitArray, BitArray) BS.ByteString
bitArrayToByteBoundary BitArray {
	bit0 = i0, bitsLen = ln, bitsBody = bs } = case i0 of
	0 -> Right bs
	_ -> Left (
		BitArray i0 (8 - i0) (BS.take 1 bs),
		BitArray 0 (ln - 8 + i0) (BS.tail bs) )

bitArrayToByteString :: BitArray -> Either BitArray BS.ByteString
bitArrayToByteString
	ba@BitArray { bit0 = i0, bitsLen = ln, bitsBody = bs } = case (i0, ln `mod` 8) of
	(0, 0) -> Right bs; _ -> Left ba

onDemand :: (
	Union.Member (
		Pipe.P BS.ByteString
			(Either BitArray BS.ByteString) ) effs,
	Union.Member (State.S Request) effs,
	Union.Member (State.S BitArray) effs,
	Union.Member (Except.E String) effs ) =>
	Eff.E effs ()
onDemand = State.get >>= \case
	RequestBytes ln -> do
		mt <- takeBytes' (Either (type BitArray) BS.ByteString) ln
		maybe (Except.throw @String "Not enough ByteString")
			(\t -> Pipe.yield BS.ByteString t >> onDemand) mt
	RequestString -> do
		mt <- takeString' (Either (type BitArray) BS.ByteString)
		maybe (Except.throw @String "Not enough ByteString")
			(\t -> Pipe.yield BS.ByteString t >> onDemand) mt
	RequestBuffer ln -> do
		mt <- takeBuffer' (Either (type BitArray) BS.ByteString) ln
		maybe (Except.throw @String "End of input")
			(\t -> Pipe.yield BS.ByteString t >> onDemand) mt
	RequestBits ln -> do
		mt <- takeBits (Either (type BitArray) BS.ByteString) ln
		maybe (Except.throw @String "Not enough ByteString")
			(\t -> Pipe.yield BS.ByteString t >> onDemand) mt

takeBytes :: forall o -> (
	Union.Member (Pipe.P BS.ByteString o) effs,
	Union.Member (State.S BS.ByteString) effs ) =>
	Int -> Eff.E effs (Maybe BS.ByteString)
takeBytes o ln = State.get >>= \bs ->
	if BS.length bs < ln
	then readMore o >>= bool (pure Nothing) (takeBytes o ln)
	else let (t, d) = BS.splitAt ln bs in Just t <$ State.put d

takeBytes' :: forall o -> (
	Union.Member (Pipe.P BS.ByteString o) effs,
	Union.Member (State.S BitArray) effs ) =>
	Int -> Eff.E effs (Maybe (Either BitArray BS.ByteString))
takeBytes' o ln = State.get >>= \ba ->
	case bitArrayToByteBoundary ba of
		Left (t, d) -> Just (Left t) <$ State.put d
		Right bs -> if BS.length bs < ln
			then readMore' o >>= bool (pure Nothing) (takeBytes' o ln)
			else let (t, d) = BS.splitAt ln bs in Just (Right t)
				<$ State.put (byteStringToBitArray d)

takeString :: forall o -> (
	Union.Member (Pipe.P BS.ByteString o) effs,
	Union.Member (State.S BS.ByteString) effs ) =>
	Eff.E effs (Maybe BS.ByteString)
takeString o = State.get >>= \bs -> case splitString bs of
	Nothing -> readMore o >>= bool (pure Nothing) (takeString o)
	Just (t, d) -> Just t <$ State.put d

takeString' :: forall o -> (
	Union.Member (Pipe.P BS.ByteString o) effs,
	Union.Member (State.S BitArray) effs ) =>
	Eff.E effs (Maybe (Either BitArray BS.ByteString))
takeString' o = State.get >>= \ba -> case bitArrayToByteBoundary ba of
	Left (t, d) -> Just (Left t) <$ State.put d
	Right bs -> case splitString bs of
		Nothing -> readMore' o >>= bool (pure Nothing) (takeString' o)
		Just (t, d) -> Just (Right t)
			<$ State.put (byteStringToBitArray d)

takeBuffer :: forall o -> (
	Union.Member (Pipe.P BS.ByteString o) effs,
	Union.Member (State.S BS.ByteString) effs ) =>
	Int -> Eff.E effs (Maybe BS.ByteString)
takeBuffer o ln = State.get >>= \bs ->
	if BS.length bs < ln
	then do	b <- readMore o
		if b then takeBuffer o ln else if BS.null bs then pure Nothing else Just bs <$ State.put ("" :: BS.ByteString)
	else let (t, d) = BS.splitAt ln bs in Just t <$ State.put d

takeBuffer' :: forall o -> (
	Union.Member (Pipe.P BS.ByteString o) effs,
	Union.Member (State.S BitArray) effs ) =>
	Int -> Eff.E effs (Maybe (Either BitArray BS.ByteString))
takeBuffer' o ln = State.get >>= \ba -> case bitArrayToByteBoundary ba of
	Left (t, d) -> Just (Left t) <$ State.put d
	Right bs -> if BS.length bs < ln
		then do	b <- readMore' o
			if b
			then takeBuffer' o ln
			else if BS.null bs
			then pure Nothing
			else Just (Right bs) <$ State.put (byteStringToBitArray ("" :: BS.ByteString))
		else let (t, d) = BS.splitAt ln bs in Just (Right t) <$ State.put (byteStringToBitArray d)

takeBits :: forall o -> (
	Union.Member (Pipe.P BS.ByteString o) effs,
	Union.Member (State.S BitArray) effs ) =>
	Int -> Eff.E effs (Maybe (Either BitArray BS.ByteString))
takeBits o ln = State.get >>= \ba -> case splitAt ln ba of
	Nothing -> readMore' o >>= bool (takeBits o ln) (pure Nothing)
	Just (t, d) -> Just (bitArrayToByteString t) <$ State.put d

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

readMore' :: forall o -> (
	Union.Member (Pipe.P BS.ByteString o) effs,
	Union.Member (State.S BitArray) effs
	) => Eff.E effs Bool
readMore' o = Pipe.await o >>= \case
	Nothing -> pure False
	Just bs -> True <$ State.modify (`appendBitArrayAndByteString` bs)

splitAt :: Int -> BitArray -> Maybe (BitArray, BitArray)
splitAt n BitArray { bit0 = i, bitsLen = ln, bitsBody = bs }
	| ln < n = Nothing
	| otherwise = Just (
		normalize $ BitArray { bit0 = i, bitsLen = n, bitsBody = bs },
		normalize $ BitArray {
			bit0 = i + n, bitsLen = ln - n, bitsBody = bs } )

normalize :: BitArray -> BitArray
normalize BitArray { bit0 = i, bitsLen = ln, bitsBody = bs }
	| 0 <= i = BitArray i' ln . BS.take t $ BS.drop (i `div` 8) bs
	| otherwise = error "bad"
	where
	i' = i `mod` 8
	t = (ln + i' - 1) `div` 8 + 1

bitArrayToWord8 :: BitArray -> Word8
bitArrayToWord8 BitArray { bit0 = i, bitsLen = ln, bitsBody = bs }
	| ln > 8 = error "too big"
	| otherwise = foldl setBit 0 $ bits i ln bs
	where
	bits i ln bs
		| ln == 0 = []
		| ln > 0 = bool id (0 :) (b `testBit` i) $ (+ 1) <$> bits i' (ln - 1) bs'
		| otherwise = error "negative length"
		where
		b = BS.head bs
		(i', bs') = case i of 7 -> (0, BS.tail bs); _ -> (i + 1, bs)
