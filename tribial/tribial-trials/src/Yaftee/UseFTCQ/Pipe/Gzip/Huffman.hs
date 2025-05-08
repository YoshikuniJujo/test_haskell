{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Yaftee.UseFTCQ.Pipe.Gzip.Huffman (

	huffmanPipe, ExtraBits(..)

	) where

import Control.Monad
import Yaftee.UseFTCQ.Eff qualified as Eff
import Yaftee.UseFTCQ.State qualified as State
import Yaftee.UseFTCQ.Pipe qualified as Pipe
import Yaftee.UseFTCQ.Fail qualified as Fail
import Yaftee.OpenUnion qualified as Union
import Data.Bits
import Data.Word

import Data.HuffmanTree (BinTree, decode1)
import Data.Bit (pattern O, pattern I)
import Data.Bit qualified as Bit

huffStep :: (Union.Member (State.S (BinTree a, BinTree a)) effs) =>
	Bit.B -> Eff.E effs i o (Maybe a)
huffStep b = do
	(t0, t) <- State.get
	let	(mr, nt) = decode1 t0 t b
	State.put (t0, nt)
	pure mr

newtype ExtraBits = ExtraBits Int deriving Show

huffmanPipe :: (
	Union.Member Pipe.P effs,
	Union.Member (State.S ExtraBits) effs,
	Union.Member (State.S (BinTree Int, BinTree Int)) effs,
	Union.Member Fail.F effs
	) =>
	Eff.E effs Bit.B (Either Int Word16) ()
huffmanPipe = do
	eb <- State.get
	case eb of
		ExtraBits 0 ->
			(\b -> (maybe (pure ()) (Pipe.yield . Left) =<< huffStep b) >> huffmanPipe)
				=<< Pipe.await
		ExtraBits n -> do
			Pipe.yield . Right =<< takeBits16' @(Either Int Word16) n
			State.put $ ExtraBits 0
			huffmanPipe

takeBits16' :: forall o effs . (
	Union.Member Pipe.P effs,
	Union.Member Fail.F effs
	) =>
	Int -> Eff.E effs Bit.B o Word16
takeBits16' n = bitsToWord16' <$> replicateM n Pipe.await

bitsToWord16' :: [Bit.B] -> Word16
bitsToWord16' = foldr (\b w -> w `shiftL` 1 .|. case b of O -> 0; I -> 1) 0
