{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Pipe.Huffman where

import Control.Arrow
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.State qualified as State
import Control.HigherOpenUnion qualified as U
import Data.Huffman
import Data.ByteString.Bit qualified as Bit

step :: forall a es i o . U.Member (State.S (BinTree a, BinTree a)) es =>
	Bit.B -> Eff.E es i o (Maybe a)
step b = State.get >>= \(t0, t) -> let
	(mr, nt) = decode1 t0 t b in
	mr <$ State.modify @(BinTree a, BinTree a) (second $ const nt)
