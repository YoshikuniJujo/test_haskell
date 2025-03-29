{-# LANGUAGE FlexibleContexts #-}

module MonadHuffman where

import Control.MonadClasses.State

import HuffmanTree

huffmanStep :: MonadState (BinTree Int, BinTree Int) m => Bit -> m (Maybe Int)
huffmanStep b = do
	(t0, t) <- get
	let	(mr, nt) = decode1 t0 t b
	put (t0, nt)
	pure mr
