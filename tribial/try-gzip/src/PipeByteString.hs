{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module PipeByteString where

import Control.Arrow
import Control.MonadClasses.State
import Data.Pipe
import Data.Word
import Data.ByteString qualified as BS

import ByteStringNum

popByte :: (
	PipeClass p, MonadState BS.ByteString (p BS.ByteString o m),
	Monad m
	) =>
	p BS.ByteString o m (Maybe Word8)
popByte = gets BS.uncons >>= \case
	Nothing -> await >>= \case
		Nothing -> pure Nothing
		Just bs -> case BS.uncons bs of
			Nothing -> popByte
			Just (b, bs') -> Just b <$ put bs'
	Just (b, bs) -> Just b <$ put bs

splitAt' :: Int -> BS.ByteString -> Maybe (BS.ByteString, BS.ByteString)
splitAt' n bs
	| BS.length bs < n = Nothing
	| otherwise = Just $ BS.splitAt n bs

takeBytes :: (
	PipeClass p, MonadState BS.ByteString (p BS.ByteString o m),
	Monad m ) => Int -> p BS.ByteString o m (Maybe BS.ByteString)
takeBytes n = gets (splitAt' n) >>= \case
	Nothing -> do
		b <- readMore
		if b then takeBytes n else pure Nothing
	Just (t, d) -> Just t <$ put d

readMore :: (
	PipeClass p, MonadState BS.ByteString (p BS.ByteString o m), Monad m ) =>
	p BS.ByteString o m Bool
readMore = await >>= \case
	Nothing -> pure False
	Just bs -> True <$ modify (`BS.append` bs)

spanUntil :: Word8 -> BS.ByteString -> Maybe (BS.ByteString, BS.ByteString)
spanUntil b0 bs = case BS.uncons bs of
	Nothing -> Nothing
	Just (b, bs')
		| b == b0 -> Just ("", bs')
		| otherwise -> ((b `BS.cons`) `first`) <$> spanUntil b0 bs'

takeString :: (
	PipeClass p, MonadState BS.ByteString (p BS.ByteString o m),
	Monad m ) =>
	p BS.ByteString o m (Maybe BS.ByteString)
takeString = gets (spanUntil 0) >>= \case
	Nothing -> do
		b <- readMore
		if b then takeString else pure Nothing
	Just (t, d) -> Just t <$ put d

takeWord32 :: (
	PipeClass p, MonadState BS.ByteString (p BS.ByteString o m),
	Monad m ) =>
	p BS.ByteString o m (Maybe Word32)
takeWord32 = (bsToNum <$>) <$> takeBytes 4
