{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MonadBitArray where

import Control.MonadClasses.State
import Control.MonadClasses.Except
import Data.Pipe
import Data.Maybe
import Data.Word
import Data.ByteString qualified as BS

import BitArray qualified as BA
import HuffmanTree

pop :: (MonadState BA.BitArray m, MonadError String m) => m Bool
pop = do
	(b, ba) <- liftEither =<< gets BA.uncons
	b <$ put ba

pop' :: (MonadState BA.BitArray m, MonadError String m) => m Bit
pop' = do
	(b, ba) <- liftEither =<< gets BA.uncons'
	b <$ put ba

pop'' :: MonadState BA.BitArray m => m (Maybe Bit)
pop'' = do
	ebba <- gets BA.uncons'
	either (const $ pure Nothing) (\(b, ba) -> Just b <$ put ba) ebba

byteBoundary ::
	(MonadState BA.BitArray m, MonadError String m) => m BA.BitArray
byteBoundary = do
	(t, d) <- liftEither =<< gets BA.splitAtByteBoundary
	t <$ put d

takeBitArray :: (MonadState BA.BitArray m, MonadError String m) =>
	Int -> m BA.BitArray
takeBitArray n = do
	(t, d) <- liftEither =<< gets (BA.splitAt n)
	t <$ put d

takeBit8 :: (MonadState BA.BitArray m, MonadError String m) =>
	Int -> m Word8
takeBit8 n = do
	(t, d) <- liftEither =<< gets (BA.splitAt n)
	w <- liftEither $ BA.bitArrayToWord8 t
	w <$ put d

bits :: (
	PipeClass p,  Monad m,
	MonadState BA.BitArray (p BS.ByteString Bit m) ) => p BS.ByteString Bit m ()
bits = do
	mb <- pop''
	case mb of
		Nothing -> do
			mbs <- await
			case mbs of
				Nothing -> pure ()
				Just bs -> do
					put $ BA.bsToBitArray bs
					yield . fromJust =<< pop''
		Just b -> yield b
