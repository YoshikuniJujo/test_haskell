{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Pipe.Huffman (
	run, States,
	huffman, Members, putTree, putExtraBits, makeTree,

	huffman', BitArray(..),

	BinTreePair, BinTree, ExtraBits(..), Pkg,

	Phase(..), IsLiteral(..)

	) where

import GHC.TypeLits
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
import Data.Bool
import Data.Huffman
import Data.Sequence qualified as Seq
import Data.Sequence.BitArray qualified as BitArray
import Data.Word

type B = Bool

{-# COMPLETE O, I #-}

pattern O, I :: Bool
pattern O = False; pattern I = True

type Pkg = "try-gzip-yaftee"

data BinTreePair a = BinTreePair (BinTree a) (BinTree a)

binTreePair :: (BinTree a, BinTree a) -> BinTreePair a
binTreePair = uncurry BinTreePair

unBinTreePair :: BinTreePair a -> (BinTree a, BinTree a)
unBinTreePair (BinTreePair x y) = (x, y)

step :: forall a es i o . forall nm -> U.Member (State.Named nm (BinTreePair a)) es =>
	B -> Eff.E es i o (Maybe a)
step nm b = State.getsN nm unBinTreePair >>= \(t0, t) -> let
	(mr, nt) = decode1 t0 t b in
	mr <$ State.modifyN @(BinTreePair a) nm (binTreePair . (second $ const nt) . unBinTreePair)

newtype ExtraBits = ExtraBits Int deriving Show

huffman :: forall a eb es r . forall nm -> Bits eb => (
	U.Member Pipe.P es,
	U.Member (State.Named nm (BinTreePair a)) es,
	U.Member (State.Named nm ExtraBits) es,
	U.Member Fail.F es ) =>
	Eff.E es B (Either a eb) r
huffman nm = State.getN nm >>= \case
	ExtraBits 0 ->
		(\b -> (maybe (pure ()) (Pipe.yield . Left) =<< step nm b) >> huffman nm)
			=<< Pipe.await
	ExtraBits n -> do
		Pipe.yield . Right =<< takeBits16' @eb @(Either a eb) n
		State.putN nm $ ExtraBits 0
		huffman nm

data Phase = PhaseLitLen | PhaseOthers deriving (Show, Eq)

newtype IsLiteral a = IsLiteral (a -> Bool)

huffman' :: forall a eb es . forall nm -> Bits eb => (
	U.Member Pipe.P es,
	U.Member (State.Named nm Phase) es,
	U.Member (State.Named nm (IsLiteral a)) es,
	U.Member (State.Named nm (BinTreePair a)) es,
	U.Member (State.Named nm ExtraBits) es,
	U.Member (State.Named nm BitArray) es,
	U.Member Fail.F es ) =>
	Eff.E es (Either BitArray.B (Seq.Seq Word8)) (Either a eb) ()
huffman' nm = State.getN nm >>= \case
	ExtraBits 0 -> do
		ph <- State.getN nm
		il <- State.getN @(IsLiteral a) nm
		if ph == PhaseLitLen
			then stepNew nm a il >>= mapM_ (Pipe.yield . Left) >> huffman' nm
			else stepNew nm a (IsLiteral $ const False) >>= mapM_ (Pipe.yield . Left) >> huffman' nm
		{-
			then getBuffer nm a >>= step' @a nm il >>= mapM_ (Pipe.yield . Left) >> huffman' nm
			else getBuffer nm a >>= step' @a nm (IsLiteral $ const False) >>= mapM_ (Pipe.yield . Left) >> huffman' nm
			-}
	ExtraBits n -> takeBits16New nm n >>= \case
		Nothing -> pure ()
		Just eb -> do
			Pipe.yield $ Right eb
			State.putN nm $ ExtraBits 0
			huffman' nm

stepNew :: forall (nm :: Symbol) a -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm (BinTreePair a)) es,
	U.Member (State.Named nm BitArray) es,
	U.Member (State.Named nm Phase) es
	) =>
	IsLiteral a ->
	Eff.E es (Either BitArray.B (Seq.Seq Word8)) o [a]
stepNew nm a (IsLiteral p) = State.getsN nm unBinTreePair >>= \(t0, t) -> do
	mr <- State.getsModifyN nm $ go t0 t
	case mr of
		Nothing -> fillBuffer nm >> stepNew nm a (IsLiteral p)
		Just (rs, nt, b) -> rs <$ do
			State.modifyN @(BinTreePair a) nm (binTreePair . (second $ const nt) . unBinTreePair)
			when b $ State.putN nm $ PhaseOthers
{-
	BitArray ba <- State.getN nm
	if BitArray.null ba
		then fillBuffer nm >> stepNew nm a (IsLiteral p)
		else do
			let	((rs, nt), (ba'', b)) = decodeBitArray p t0 t ba
			rs <$ do
				State.modifyN @(BinTreePair a) nm (binTreePair . (second $ const nt) . unBinTreePair)
				State.putN nm (BitArray ba'')
				when b $ State.put PhaseOthers
				-}
	where
	go t0 t ba = if BitArray.null $ unBitArray ba
		then Nothing
		else let	((rs, nt), (ba'', b)) = decodeBitArray p t0 t $ unBitArray ba in
				Just ((rs, nt, b), BitArray ba'')

fillBuffer :: forall (nm :: Symbol) -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm BitArray) es ) =>
	Eff.E es (Either BitArray.B (Seq.Seq Word8)) o ()
fillBuffer nm = State.putN nm . BitArray . either id BitArray.fromSequence =<< Pipe.await

readMore :: forall (nm :: Symbol) -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm BitArray) es
	) =>
	Eff.E es (Either BitArray.B (Seq.Seq Word8)) o Bool
readMore nm = Pipe.awaitMaybe >>= \case
	Nothing -> pure False
	Just (either id BitArray.fromSequence -> ba)
		| BitArray.null ba -> pure False
		| otherwise -> True <$ State.modifyN nm (`appendBitArray` ba)

newtype BitArray = BitArray { unBitArray :: BitArray.B } deriving Show

appendBitArray :: BitArray -> BitArray.B -> BitArray
appendBitArray (BitArray ba) ba' = BitArray $ ba `BitArray.append` ba'


type Members nm a es = (
	U.Member (State.Named nm (BinTreePair a)) es,
	U.Member (State.Named nm ExtraBits) es,
	U.Member (State.Named nm BitArray) es,
	U.Member (State.Named nm Phase) es,
	U.Member (State.Named nm (IsLiteral a)) es )

run :: forall nm a es i o r . (HFunctor.Loose (U.U es), Ord a) =>
	Eff.E (States nm a `Append` es) i o r ->
	Eff.E es i o (
		((((r, BinTreePair a), ExtraBits), BitArray), Phase),
		IsLiteral a )
run = (`State.runN` IsLiteral (const False))
	. (`State.runN` PhaseOthers)
	. (`State.runN` BitArray BitArray.empty)
	. flip (State.runN @nm) (ExtraBits 0)
	. flip (State.runN @nm) (BinTreePair bt bt)
	where bt = makeTree [] ([] :: [Int])

type States nm a = '[
	State.Named nm (BinTreePair a),
	State.Named nm ExtraBits,
	State.Named nm BitArray,
	State.Named nm Phase,
	State.Named nm (IsLiteral a) ]

putTree :: forall a es i o . forall nm -> U.Member (State.Named nm (BinTreePair a)) es =>
	BinTree a -> Eff.E es i o ()
putTree nm tr = State.putN nm (BinTreePair tr tr)

putExtraBits ::
	forall nm -> U.Member (State.Named nm ExtraBits) es => Int -> Eff.E es i o ()
putExtraBits nm = State.putN nm . ExtraBits

takeBits16' :: forall eb o es . Bits eb => U.Member Pipe.P es =>
	Int -> Eff.E es B o eb
takeBits16' n = bitsToWord16' <$> replicateM n Pipe.await

bitsToWord16' :: Bits eb => [B] -> eb
bitsToWord16' = foldr (\b w -> w `shiftL` 1 .|. case b of O -> zeroBits; I -> bit 0) zeroBits

takeBits16New :: forall eb o es . forall (nm :: Symbol) -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm BitArray) es,
	Bits eb
	) =>
	Int -> Eff.E es (Either BitArray.B (Seq.Seq Word8)) o (Maybe eb)
takeBits16New nm n = State.getsModifyN nm go >>= \case
	Nothing -> readMore nm >>= bool (pure Nothing) (takeBits16New nm n)
	Just eb -> pure $ Just eb
	where
	go ba = case BitArray.popBits n $ unBitArray ba of
		Nothing -> Nothing
		Just (eb, ba') -> Just (eb, BitArray ba')
