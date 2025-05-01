{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Fail (F, run, runExc, runExcN) where

import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.HigherFreer qualified as F
import Control.HigherOpenUnion qualified as U
import Data.Functor.Identity
import Data.HigherFunctor qualified as HFunctor
import Data.FTCQueue qualified as Q

type F = U.FromFirst U.Fail

run :: HFunctor.Loose (U.U effs) =>
	Eff.E (F ': effs) i o a -> Eff.E effs i o (Either String a)
run = \case
	F.Pure x -> F.Pure $ Right x
	u F.:>>= q -> case U.decomp u of
		Left u' -> HFunctor.map run Right u' F.:>>=
			Q.singleton (either (F.Pure . Left) (run F.. q))
		Right (U.FromFirst (U.Fail m) _) -> F.Pure $ Left m

runExc :: (HFunctor.Loose (U.U effs), U.Member (Except.E e) effs) =>
	(String -> e) -> Eff.E (F ': effs) i o a -> Eff.E effs i o a
runExc = runExcN ""

runExcN :: forall e effs i o a . forall nm -> (
	HFunctor.Loose (U.U effs),
	U.Member (Except.Named nm e) effs ) =>
	(String -> e) -> Eff.E (F ': effs) i o a -> Eff.E effs i o a
runExcN nm err = \case
	F.Pure x -> F.Pure x
	u F.:>>= q -> case U.decomp u of
		Left u' -> HFunctor.map ((Identity <$>) . runExcN nm err)
				Identity u' F.:>>=
			Q.singleton (runExcN nm err . (q F.$) . runIdentity)
		Right (U.FromFirst (U.Fail m) _) -> Except.throwN nm $ err m
