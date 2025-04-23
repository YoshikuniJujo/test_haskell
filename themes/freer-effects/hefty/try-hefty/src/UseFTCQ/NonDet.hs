{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module UseFTCQ.NonDet where

import Control.Applicative
import Control.Monad
import UseFTCQ.Eff qualified as Eff
import UseFTCQ.HFreer qualified as HFreer
import OpenUnion qualified as Union
import Data.FTCQueue qualified as Q

type N = Union.FromFirst Union.NonDet

run :: (
	Union.HFunctor (Union.U effs),
	Traversable f, Alternative f, Monad f
	) => Eff.E (N ': effs) a -> Eff.E effs (f a)
run = \case
	HFreer.Pure x -> HFreer.Pure (pure x)
	u HFreer.:>>= q -> case Union.decomp u of
		Left u' -> Union.hmap run pure u' HFreer.:>>=
			Q.singleton \xs -> join <$> run (traverse (q `HFreer.app`) xs)
		Right (Union.FromFirst Union.MZero) -> pure empty
		Right (Union.FromFirst Union.MPlus) ->
			liftA2 (<|>) (run $ q `HFreer.app` False) (run $ q `HFreer.app` True)

split :: Union.Member N effs =>
	Eff.E effs a -> Eff.E effs (Maybe (a, Eff.E effs a))
split = go []
	where
	go jq (HFreer.Pure x) = pure (Just (x, msum jq))
	go jq (u HFreer.:>>= q) = case Union.prj u of
		Nothing -> u HFreer.:>>= Q.singleton (go jq `HFreer.comp` q)
		Just (Union.FromFirst Union.MZero) -> case jq of
			[] -> pure Nothing
			j : jq' -> go jq' j
		Just (Union.FromFirst Union.MPlus) -> go (q `HFreer.app` True : jq) (q `HFreer.app` False)

type KnightPos = (Int, Int)

moveKnight :: Union.Member N effs => KnightPos -> Eff.E effs KnightPos
moveKnight (c, r) = do
	(c', r') <- asum $ pure <$> [
		(c + 2, r - 1), (c + 2, r + 1), (c - 2, r - 1), (c - 2, r + 1),
		(c + 1, r - 2), (c + 1, r + 2), (c - 1, r - 2), (c - 1, r + 2) ]
	guard $ c' `elem` [1 .. 8] && r' `elem` [1 .. 8]
	pure (c', r')

in3 :: Union.Member N effs => KnightPos -> Eff.E effs KnightPos
in3 st = do
	fs <- moveKnight st
	sc <- moveKnight fs
	moveKnight sc

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 st ed = ed `elem` (Eff.run . run $ in3 st :: [KnightPos])
