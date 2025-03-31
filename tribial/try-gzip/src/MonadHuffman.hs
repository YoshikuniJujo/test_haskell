{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MonadHuffman where

import Control.Monad
import Control.MonadClasses.State
import Data.Bits
import Data.Pipe
import Data.Maybe
import Data.Word

import HuffmanTree

huffStep :: MonadState (BinTree Int, BinTree Int) m => Bit -> m (Maybe Int)
huffStep b = do
	(t0, t) <- get
	let	(mr, nt) = decode1 t0 t b
	put (t0, nt)
	pure mr

newtype ExtraBits = ExtraBits Int deriving Show

huffmanPipe :: (
	PipeClass p, Monad m,
	MonadState (BinTree Int, BinTree Int) (p Bit (Either Int Word16) m),
	MonadState ExtraBits (p Bit (Either Int Word16) m)
	) =>
	p Bit (Either Int Word16) m ()
huffmanPipe = do
	eb <- get
	case eb of
		ExtraBits 0 ->
			maybe (pure ()) (\b -> maybe (pure ()) (yield . Left) =<< huffStep b) =<< await
		ExtraBits n -> do
			yield . Right =<< takeBits16' n
			put $ ExtraBits 0
	huffmanPipe

takeBits16 n = bitsToWord16 <$> replicateM n (fromJust <$> await)
takeBits16' n = bitsToWord16' <$> replicateM n (fromJust <$> await)

bitsToWord16 :: [Bit] -> Word16
bitsToWord16 = foldl (\w b -> w `shiftL` 1 .|. case b of O -> 0; I -> 1) 0

bitsToWord16' :: [Bit] -> Word16
bitsToWord16' = foldr (\b w -> w `shiftL` 1 .|. case b of O -> 0; I -> 1) 0
