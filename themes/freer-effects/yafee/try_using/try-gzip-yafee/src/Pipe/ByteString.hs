{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Pipe.ByteString where

import Control.Monad.Yafee.Eff qualified as Eff
import Control.Monad.Yafee.State qualified as State
import Control.Monad.Yafee.Pipe qualified as Pipe
import Control.OpenUnion qualified as Union
import Data.Word
import Data.ByteString qualified as BS

import BitArray

popByte :: forall o effs . (
	Union.Member (State.S BS.ByteString) effs,
	Union.Member (State.S BitInfo) effs,
	Union.Member (Pipe.P BS.ByteString o) effs ) =>
	Eff.E effs (Maybe Word8)
popByte = State.gets BS.uncons >>= \case
	Nothing -> Pipe.await @_ @o >>= \case
		Nothing -> pure Nothing
		Just bs -> case BS.uncons bs of
			Nothing -> popByte @o
			Just (b, bs') -> Just b <$ do
				State.put bs'
				State.put $ BitInfo 0 (BS.length bs' * 8)
	Just (b, bs) -> Just b <$ do
		State.put bs
		State.put $ BitInfo 0 (BS.length bs * 8)

takeBytes :: forall o effs . (
	Union.Member (State.S BS.ByteString) effs,
	Union.Member (State.S BitInfo) effs,
	Union.Member (Pipe.P BS.ByteString o) effs ) =>
	Int -> Eff.E effs (Maybe BS.ByteString)
takeBytes n = State.gets (splitAt' n) >>= \case
	Nothing -> do
		b <- readMore @o
		if b then takeBytes @o n else pure Nothing
	Just (t, d) -> Just t <$ do
		State.put d
		State.put $ BitInfo 0 (BS.length d * 8)

splitAt' :: Int -> BS.ByteString -> Maybe (BS.ByteString, BS.ByteString)
splitAt' n bs
	| BS.length bs < n = Nothing
	| otherwise = Just $ BS.splitAt n bs
