{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Pipe.Huffman where

import Control.Monad
import Control.Monad.Yafe.Eff qualified as Eff
import Control.Monad.Yafee.State
import Control.Monad.Yafe.Pipe
import Control.Monad.Yafee.Fail qualified as Fail
import Control.OpenUnion qualified as Union
import Data.Bits
import Data.Word

import HuffmanTree
import BitArray

huffStep :: (Union.Member (State (BinTree a, BinTree a)) effs) =>
	Bit -> Eff.E effs (Maybe a)
huffStep b = do
	(t0, t) <- get
	let	(mr, nt) = decode1 t0 t b
	put (t0, nt)
	pure mr

newtype ExtraBits = ExtraBits Int deriving Show

huffmanPipe :: (
	Union.Member (State ExtraBits) effs,
	Union.Member (State (BinTree Int, BinTree Int)) effs,
	Union.Member (Pipe Bit (Either Int Word16)) effs,
	Union.Member Fail.F effs
	) =>
	Eff.E effs ()
huffmanPipe = do
	eb <- get
	case eb of
		ExtraBits 0 ->
			maybe (pure ())
				(\b -> (maybe (pure ()) (yield @Bit @(Either Int Word16) . Left) =<< huffStep b) >> huffmanPipe)
				=<< await @Bit @(Either Int Word16)
		ExtraBits n -> do
			yield @Bit @(Either Int Word16) . Right =<< takeBits16' @(Either Int Word16) n
			put $ ExtraBits 0
			huffmanPipe

takeBits16, takeBits16' :: forall o effs . (
	Union.Member (Pipe Bit o) effs,
	Union.Member Fail.F effs
	) =>
	Int -> Eff.E effs Word16
takeBits16 n = bitsToWord16 <$> replicateM n (maybe (fail "bad") pure =<< await @_ @o)
takeBits16' n = bitsToWord16' <$> replicateM n (maybe (fail "bad") pure =<< await @_ @o)

bitsToWord16 :: [Bit] -> Word16
bitsToWord16 = foldl (\w b -> w `shiftL` 1 .|. case b of O -> 0; I -> 1) 0

bitsToWord16' :: [Bit] -> Word16
bitsToWord16' = foldr (\b w -> w `shiftL` 1 .|. case b of O -> 0; I -> 1) 0
