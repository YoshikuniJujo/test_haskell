{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Yaftee.UseFTCQ.NonDet where

import Control.Applicative
import Control.Monad
import Yaftee.UseFTCQ.Eff qualified as Eff
import Yaftee.UseFTCQ.HFreer qualified as F
import Yaftee.OpenUnion qualified as Union
import Yaftee.HFunctor qualified as Union
import Data.FTCQueue qualified as Q

type N = Union.NonDet

once :: Union.Member Union.NonDet effs =>
	Eff.E effs i o a -> Eff.E effs i o a
once = Eff.effh . Union.Once

run :: forall f effs i o a .
	(Union.HFunctor (Union.U effs), Traversable f, MonadPlus f) =>
	(forall x . f x -> Maybe x) -> Eff.E (N ': effs) i o a -> Eff.E effs i o (f a)
run c = \case
	F.Pure x -> F.Pure $ pure x
	u F.:>>= q -> case Union.decomp u of
		Left u' -> Union.hmap (run c) pure u' F.:>>=
			Q.singleton ((join <$>) . ((run c . (q `F.app`)) `traverse`))
		Right Union.MZero -> pure empty
		Right (Union.MPlus k) ->
			(<|>) <$> run c (q `F.app` k False) <*> run c (q `F.app` k True)
		Right (Union.Once m) -> maybe (pure empty) (run c `F.comp` q) . c =<< run c m

type KnightPos = (Int, Int)

-- moveKnight :: KnightPos -> [KnightPos]
moveKnight (c, r) = do
	(c', r') <- asum $ pure <$> [
		(c + 2, r - 1), (c + 2, r + 1), (c - 2, r - 1), (c - 2, r + 1),
		(c + 1, r - 2), (c + 1, r + 2), (c - 1, r - 2), (c - 1, r + 2) ]
	guard $ c' `elem` [1 .. 8] && r' `elem` [1 .. 8]
	pure (c', r')

in3 st = do
	sd <- once do
		ft <- moveKnight st
		moveKnight ft
	moveKnight sd
