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
import Data.Bits
import Data.Word
import Data.ByteString qualified as BS

import BitArray

import Crc qualified as Crc

import Numeric

popByte :: forall o effs . (
	Union.Member (State.S BitArray) effs,
	Union.Member (Pipe.P BS.ByteString o) effs ) =>
	Eff.E effs (Maybe Word8)
popByte = State.gets (BS.uncons . bitsBody) >>= \case
	Nothing -> Pipe.await' o >>= \case
		Nothing -> pure Nothing
		Just bs -> case BS.uncons bs of
			Nothing -> popByte @o
			Just (b, bs') -> Just b
				<$ State.put (BitArray 0 (BS.length bs' * 8) bs')
	Just (b, bs) -> Just b
		<$ State.put (BitArray 0 (BS.length bs * 8) bs)

popByte' :: forall o effs . (
	Union.Member (State.S BitArray) effs,
	Union.Member (State.S Crc) effs,
	Union.Member (Pipe.P BS.ByteString o) effs
	) =>
	Eff.E effs (Maybe Word8)
popByte' = popByte @o >>= \mw -> mw <$ maybe (pure ()) calcCrc mw

takeBytes :: forall o effs . (
	Union.Member (State.S BitArray) effs,
	Union.Member (Pipe.P BS.ByteString o) effs ) =>
	Int -> Eff.E effs (Maybe BS.ByteString)
takeBytes n = State.gets (splitAt' n . bitsBody) >>= \case
	Nothing -> do
		b <- readMore @o
		if b then takeBytes @o n else pure Nothing
	Just (t, d) -> Just t <$ State.put (BitArray 0 (BS.length d * 8) d)

takeBytes' :: forall o effs . (
	Union.Member (State.S BitArray) effs,
	Union.Member (State.S Crc) effs,
	Union.Member (Pipe.P BS.ByteString o) effs
	) =>
	Int -> Eff.E effs (Maybe BS.ByteString)
takeBytes' n = takeBytes @o n >>= \mw -> mw <$ maybe (pure ()) calcCrc' mw

splitAt' :: Int -> BS.ByteString -> Maybe (BS.ByteString, BS.ByteString)
splitAt' n bs
	| BS.length bs < n = Nothing
	| otherwise = Just $ BS.splitAt n bs

newtype Crc = Crc Word32

instance Show Crc where
	show (Crc w) = "(Crc " ++ showHex w "" ++ ")"

compCrc :: Crc -> Crc
compCrc (Crc c) = Crc $ complement c

step :: Word8 -> Crc -> Crc
step w (Crc c) = Crc $ Crc.step c w

calcCrc :: forall effs .
	(Union.Member (State.S Crc) effs ) => Word8 -> Eff.E effs ()
calcCrc = State.modify . step

step' :: BS.ByteString -> Crc -> Crc
step' bs (Crc c) = Crc $ Crc.step' c bs

calcCrc' :: forall effs . (
	Union.Member (State.S Crc) effs ) =>
	BS.ByteString -> Eff.E effs ()
calcCrc' bs = State.modify $ step' bs
