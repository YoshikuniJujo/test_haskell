{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Pipe.Huffman (
	run, States,
	huffman, Members, putTree, putExtraBits, makeTree,

	BinTreePair, BinTree, ExtraBits(..), Pkg,

	) where

import Control.Arrow
import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Fail qualified as Fail
import Control.HigherOpenUnion qualified as U
import Data.TypeLevel.List
import Data.HigherFunctor qualified as HFunctor
import Data.Bits
import Data.Huffman
import Data.ByteString.Bit (pattern O, pattern I)
import Data.ByteString.Bit qualified as Bit

type Pkg = "try-gzip-yaftee"

data BinTreePair a = BinTreePair (BinTree a) (BinTree a)

binTreePair :: (BinTree a, BinTree a) -> BinTreePair a
binTreePair = uncurry BinTreePair

unBinTreePair :: BinTreePair a -> (BinTree a, BinTree a)
unBinTreePair (BinTreePair x y) = (x, y)

step :: forall a es i o . forall nm -> U.Member (State.Named nm (BinTreePair a)) es =>
	Bit.B -> Eff.E es i o (Maybe a)
step nm b = State.getsN nm unBinTreePair >>= \(t0, t) -> let
	(mr, nt) = decode1 t0 t b in
	mr <$ State.modifyN @(BinTreePair a) nm (binTreePair . (second $ const nt) . unBinTreePair)

newtype ExtraBits = ExtraBits Int deriving Show

huffman :: forall a eb es r . forall nm -> Bits eb => (
	U.Member Pipe.P es,
	U.Member (State.Named nm (BinTreePair a)) es,
	U.Member (State.Named nm ExtraBits) es,
	U.Member Fail.F es ) =>
	Eff.E es Bit.B (Either a eb) r
huffman nm = do
	eb <- State.getN nm
	case eb of
		ExtraBits 0 ->
			(\b -> (maybe (pure ()) (Pipe.yield . Left) =<< step nm b) >> huffman nm)
				=<< Pipe.await
		ExtraBits n -> do
			Pipe.yield . Right =<< takeBits16' @eb @(Either a eb) n
			State.putN nm $ ExtraBits 0
			huffman nm

type Members nm a es = (
	U.Member (State.Named nm (BinTreePair a)) es,
	U.Member (State.Named nm ExtraBits) es )

run :: forall nm a es i o r . (HFunctor.Loose (U.U es), Ord a) =>
	Eff.E (States nm a `Append` es) i o r ->
	Eff.E es i o ((r, ExtraBits), (BinTreePair a))
run = flip (State.runN @nm) (BinTreePair bt bt) . flip (State.runN @nm) (ExtraBits 0)
	where bt = makeTree [] ([] :: [Int])

type States nm a = '[State.Named nm ExtraBits, State.Named nm (BinTreePair a)]

putTree :: forall a es i o . forall nm -> U.Member (State.Named nm (BinTreePair a)) es =>
	BinTree a -> Eff.E es i o ()
putTree nm tr = State.putN nm (BinTreePair tr tr)

putExtraBits ::
	forall nm -> U.Member (State.Named nm ExtraBits) es => Int -> Eff.E es i o ()
putExtraBits nm = State.putN nm . ExtraBits

takeBits16' :: forall eb o es . Bits eb => U.Member Pipe.P es =>
	Int -> Eff.E es Bit.B o eb
takeBits16' n = bitsToWord16' <$> replicateM n Pipe.await

bitsToWord16' :: Bits eb => [Bit.B] -> eb
bitsToWord16' = foldr (\b w -> w `shiftL` 1 .|. case b of O -> zeroBits; I -> bit 0) zeroBits
