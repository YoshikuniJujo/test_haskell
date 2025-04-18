{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Pipe.Huffman (

	huffmanPipe, ExtraBits(..)

	) where

import Control.Monad
import Control.Monad.Yafee.Eff qualified as Eff
import Control.Monad.Yafee.State qualified as State
import Control.Monad.Yafee.Pipe qualified as Pipe
import Control.Monad.Yafee.Fail qualified as Fail
import Control.OpenUnion qualified as Union
import Data.Bits
import Data.Word

import HuffmanTree
import BitArray

huffStep :: (Union.Member (State.S (BinTree a, BinTree a)) effs) =>
	Bit -> Eff.E effs (Maybe a)
huffStep b = do
	(t0, t) <- State.get
	let	(mr, nt) = decode1 t0 t b
	State.put (t0, nt)
	pure mr

newtype ExtraBits = ExtraBits Int deriving Show

huffmanPipe :: (
	Union.Member (State.S ExtraBits) effs,
	Union.Member (State.S (BinTree Int, BinTree Int)) effs,
	Union.Member (Pipe.P Bit (Either Int Word16)) effs,
	Union.Member Fail.F effs
	) =>
	Eff.E effs ()
huffmanPipe = do
	eb <- State.get
	case eb of
		ExtraBits 0 ->
			maybe (pure ())
				(\b -> (maybe (pure ()) (Pipe.yield' @(Either Int Word16) Bit . Left) =<< huffStep b) >> huffmanPipe)
				=<< Pipe.await' @Bit (Either Int Word16)
		ExtraBits n -> do
			Pipe.yield' @(Either Int Word16) Bit . Right =<< takeBits16' @(Either Int Word16) n
			State.put $ ExtraBits 0
			huffmanPipe

takeBits16' :: forall o effs . (
	Union.Member (Pipe.P Bit o) effs,
	Union.Member Fail.F effs
	) =>
	Int -> Eff.E effs Word16
takeBits16' n = bitsToWord16' <$> replicateM n (maybe (fail "takeBits16': bad") pure =<< Pipe.await' o)

bitsToWord16' :: [Bit] -> Word16
bitsToWord16' = foldr (\b w -> w `shiftL` 1 .|. case b of O -> 0; I -> 1) 0
