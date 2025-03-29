{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MonadBitArray where

import Control.MonadClasses.State
import Control.MonadClasses.Except
import Data.Word

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
