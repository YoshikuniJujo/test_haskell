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
	Union.Member (State.S Request) effs,
	Union.Member (State.S BitArray) effs,
	Union.Member (Except.E String) effs ) =>
	Eff.E (Pipe.P BS.ByteString (Either BitArray BS.ByteString) ': effs) ()
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

takeBytes :: (
	Union.Member (State.S BS.ByteString) effs ) =>
	Int -> Eff.E (Pipe.P BS.ByteString o ': effs) (Maybe BS.ByteString)
takeBytes ln = State.get >>= \bs ->
	if BS.length bs < ln
	then readMore >>= bool (pure Nothing) (takeBytes ln)
	else let (t, d) = BS.splitAt ln bs in Just t <$ State.put d

takeBytes' :: (
	Union.Member (State.S BitArray) effs ) =>
	Int -> Eff.E (Pipe.P BS.ByteString o ': effs) (Maybe (Either BitArray BS.ByteString))
takeBytes' ln = State.get >>= \ba ->
	case bitArrayToByteBoundary ba of
		Left (t, d) -> Just (Left t) <$ State.put d
		Right bs -> if BS.length bs < ln
			then readMore' >>= bool (pure Nothing) (takeBytes' ln)
			else let (t, d) = BS.splitAt ln bs in Just (Right t)
				<$ State.put (byteStringToBitArray d)

takeString :: (
	Union.Member (State.S BS.ByteString) effs ) =>
	Eff.E (Pipe.P BS.ByteString o ': effs) (Maybe BS.ByteString)
takeString = State.get >>= \bs -> case splitString bs of
	Nothing -> readMore >>= bool (pure Nothing) takeString
	Just (t, d) -> Just t <$ State.put d

takeString' :: (
	Union.Member (State.S BitArray) effs ) =>
	Eff.E (Pipe.P BS.ByteString o ': effs) (Maybe (Either BitArray BS.ByteString))
takeString' = State.get >>= \ba -> case bitArrayToByteBoundary ba of
	Left (t, d) -> Just (Left t) <$ State.put d
	Right bs -> case splitString bs of
		Nothing -> readMore' >>= bool (pure Nothing) takeString'
		Just (t, d) -> Just (Right t)
			<$ State.put (byteStringToBitArray d)

takeBuffer :: (
	Union.Member (State.S BS.ByteString) effs ) =>
	Int -> Eff.E (Pipe.P BS.ByteString o ': effs) (Maybe BS.ByteString)
takeBuffer ln = State.get >>= \bs ->
	if BS.length bs < ln
	then do	b <- readMore
		if b then takeBuffer ln else if BS.null bs then pure Nothing else Just bs <$ State.put ("" :: BS.ByteString)
	else let (t, d) = BS.splitAt ln bs in Just t <$ State.put d

takeBuffer' :: (
	Union.Member (State.S BitArray) effs ) =>
	Int -> Eff.E (Pipe.P BS.ByteString o ': effs) (Maybe (Either BitArray BS.ByteString))
takeBuffer' ln = State.get >>= \ba -> case bitArrayToByteBoundary ba of
	Left (t, d) -> Just (Left t) <$ State.put d
	Right bs -> if BS.length bs < ln
		then do	b <- readMore'
			if b
			then takeBuffer' ln
			else if BS.null bs
			then pure Nothing
			else Just (Right bs) <$ State.put (byteStringToBitArray ("" :: BS.ByteString))
		else let (t, d) = BS.splitAt ln bs in Just (Right t) <$ State.put (byteStringToBitArray d)

takeBits :: Union.Member (State.S BitArray) effs =>
	Int -> Eff.E (Pipe.P BS.ByteString o ': effs) (Maybe (Either BitArray BS.ByteString))
takeBits ln = State.get >>= \ba -> case splitAt ln ba of
	Nothing -> readMore' >>= bool (takeBits ln) (pure Nothing)
	Just (t, d) -> Just (bitArrayToByteString t) <$ State.put d

splitString :: BS.ByteString -> Maybe (BS.ByteString, BS.ByteString)
splitString bs = case BS.span (/= 0) bs of
	(_, "") -> Nothing
	(t, BS.uncons -> Just (z, d)) -> Just (BS.snoc t z, d)

readMore :: (
	Union.Member (State.S BS.ByteString) effs
	) =>
	Eff.E (Pipe.P BS.ByteString o ': effs) Bool
readMore = Pipe.await >>= \case
	Nothing -> pure False
	Just bs -> True <$ State.modify (`BS.append` bs)

readMore' :: (
	Union.Member (State.S BitArray) effs
	) => Eff.E (Pipe.P BS.ByteString o ': effs) Bool
readMore' = Pipe.await >>= \case
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
bitArrayToWord8 BitArray { bit0 = i0, bitsLen = ln0, bitsBody = bs0 }
	| ln0 > 8 = error "too big"
	| otherwise = foldl setBit 0 $ bits i0 ln0 bs0
	where
	bits i ln bs
		| ln == 0 = []
		| ln > 0 = bool id (0 :) (b `testBit` i) $ (+ 1) <$> bits i' (ln - 1) bs'
		| otherwise = error "negative length"
		where
		b = BS.head bs
		(i', bs') = case i of 7 -> (0, BS.tail bs); _ -> (i + 1, bs)
