{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Pipe.Huffman (huffman, ExtraBits(..), Pkg) where

import Control.Arrow
import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Fail qualified as Fail
import Control.HigherOpenUnion qualified as U
import Data.Bits
import Data.Huffman
import Data.Word
import Data.ByteString.Bit (pattern O, pattern I)
import Data.ByteString.Bit qualified as Bit

type Pkg = "try-gzip-yaftee"

step :: forall a es i o . U.Member (State.Named Pkg (BinTree a, BinTree a)) es =>
	Bit.B -> Eff.E es i o (Maybe a)
step b = State.getN Pkg >>= \(t0, t) -> let
	(mr, nt) = decode1 t0 t b in
	mr <$ State.modifyN @(BinTree a, BinTree a) Pkg (second $ const nt)

newtype ExtraBits = ExtraBits Int deriving Show

huffman :: (
	U.Member Pipe.P es,
	U.Member (State.Named Pkg (BinTree Int, BinTree Int)) es,
	U.Member (State.Named Pkg ExtraBits) es,
	U.Member Fail.F es ) =>
	Eff.E es Bit.B (Either Int Word16) r
huffman = do
	eb <- State.getN Pkg
	case eb of
		ExtraBits 0 ->
			(\b -> (maybe (pure ()) (Pipe.yield . Left) =<< step b) >> huffman)
				=<< Pipe.await
		ExtraBits n -> do
			Pipe.yield . Right =<< takeBits16' @(Either Int Word16) n
			State.putN Pkg $ ExtraBits 0
			huffman

takeBits16' :: forall o es . U.Member Pipe.P es =>
	Int -> Eff.E es Bit.B o Word16
takeBits16' n = bitsToWord16' <$> replicateM n Pipe.await

bitsToWord16' :: [Bit.B] -> Word16
bitsToWord16' = foldr (\b w -> w `shiftL` 1 .|. case b of O -> 0; I -> 1) 0
