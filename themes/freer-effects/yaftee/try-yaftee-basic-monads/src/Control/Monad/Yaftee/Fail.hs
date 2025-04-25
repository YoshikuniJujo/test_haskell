{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Fail where

import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.HFreer qualified as HFreer
import Control.HigherOpenUnion qualified as Union
import Data.HFunctor qualified as HFunctor
import Data.FTCQueue qualified as Q

type F = Union.FromFirst Union.Fail

run :: HFunctor.H (Union.U effs) =>
	Eff.E (F ': effs) a -> Eff.E effs (Either String a)
run = \case
	HFreer.Pure x -> HFreer.Pure $ Right x
	u HFreer.:>>= q -> case Union.decomp u of
		Left u' -> HFunctor.map run Right u' HFreer.:>>= Q.singleton
			(either (HFreer.Pure . Left) (run `HFreer.comp` q))
		Right (Union.FromFirst (Union.Fail m) _) -> HFreer.Pure $ Left m

runExc :: (HFunctor.H (Union.U effs), Union.Member (Except.E e) effs) =>
	(String -> e) -> Eff.E (F ': effs) a -> Eff.E effs a
runExc err = \case
	HFreer.Pure x -> HFreer.Pure x
	u HFreer.:>>= q -> case Union.decomp u of
		Left u' -> HFunctor.map run Right u' HFreer.:>>= Q.singleton
			(either (Except.throw . err) (runExc err `HFreer.comp` q))
		Right (Union.FromFirst (Union.Fail m) _) -> Except.throw $ err m
