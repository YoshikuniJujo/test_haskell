{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Pipe.Huffman (
	run, huffman, putTree, putExtraBits, makeTree,

	BinTree, ExtraBits(..), Pkg,

	) where

import Control.Arrow
import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Fail qualified as Fail
import Control.HigherOpenUnion qualified as U
import Data.HigherFunctor qualified as HFunctor
import Data.Bits
import Data.Huffman
import Data.ByteString.Bit (pattern O, pattern I)
import Data.ByteString.Bit qualified as Bit

type Pkg = "try-gzip-yaftee"

step :: forall a es i o . U.Member (State.Named Pkg (BinTree a, BinTree a)) es =>
	Bit.B -> Eff.E es i o (Maybe a)
step b = State.getN Pkg >>= \(t0, t) -> let
	(mr, nt) = decode1 t0 t b in
	mr <$ State.modifyN @(BinTree a, BinTree a) Pkg (second $ const nt)

newtype ExtraBits = ExtraBits Int deriving Show

huffman :: forall a eb es r . Bits eb => (
	U.Member Pipe.P es,
	U.Member (State.Named Pkg (BinTree a, BinTree a)) es,
	U.Member (State.Named Pkg ExtraBits) es,
	U.Member Fail.F es ) =>
	Eff.E es Bit.B (Either a eb) r
huffman = do
	eb <- State.getN Pkg
	case eb of
		ExtraBits 0 ->
			(\b -> (maybe (pure ()) (Pipe.yield . Left) =<< step b) >> huffman)
				=<< Pipe.await
		ExtraBits n -> do
			Pipe.yield . Right =<< takeBits16' @eb @(Either a eb) n
			State.putN Pkg $ ExtraBits 0
			huffman

run :: HFunctor.Loose (U.U es) =>
	BinTree a ->
	Eff.E (State.Named Pkg ExtraBits ': State.Named Pkg (BinTree a, BinTree a) ': es) i o r ->
	Eff.E es i o ((r, ExtraBits), (BinTree a, BinTree a))
run bt = flip (State.runN @Pkg) (bt, bt) . flip (State.runN @Pkg) (ExtraBits 0)

putTree :: U.Member (State.Named Pkg (BinTree a, BinTree a)) es =>
	BinTree a -> Eff.E es i o ()
putTree tr = State.putN Pkg (tr, tr)

putExtraBits ::
	U.Member (State.Named Pkg ExtraBits) es => Int -> Eff.E es i o ()
putExtraBits = State.putN Pkg . ExtraBits

takeBits16' :: forall eb o es . Bits eb => U.Member Pipe.P es =>
	Int -> Eff.E es Bit.B o eb
takeBits16' n = bitsToWord16' <$> replicateM n Pipe.await

bitsToWord16' :: Bits eb => [Bit.B] -> eb
bitsToWord16' = foldr (\b w -> w `shiftL` 1 .|. case b of O -> zeroBits; I -> bit 0) zeroBits
