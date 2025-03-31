{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MonadHuffman where

import Control.MonadClasses.State
import Data.Pipe

import HuffmanTree

huffStep :: MonadState (BinTree Int, BinTree Int) m => Bit -> m (Maybe Int)
huffStep b = do
	(t0, t) <- get
	let	(mr, nt) = decode1 t0 t b
	put (t0, nt)
	pure mr

huffmanPipe :: (
	PipeClass p, Monad m,
	MonadState (BinTree Int, BinTree Int) (p Bit Int m)) =>
	p Bit Int m ()
huffmanPipe = do
	maybe (pure ()) (\b -> maybe (pure ()) yield =<< huffStep b) =<< await
	huffmanPipe
