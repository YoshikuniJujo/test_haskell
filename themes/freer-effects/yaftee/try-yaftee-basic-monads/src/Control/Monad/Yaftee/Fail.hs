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

type F = U.Fail

run :: HFunctor.Loose (U.U effs) =>
	Eff.E (F ': effs) i o a -> Eff.E effs i o (Either String a)
run = \case
	F.Pure x -> F.Pure $ Right x
	u F.:>>= q -> case U.decomp u of
		Left u' -> HFunctor.map run Right u' F.:>>=
			Q.singleton (either (F.Pure . Left) (run F.. q))
		Right (U.Fail m) -> F.Pure $ Left m
		Right (m `U.FailCatch` h) -> either (F.Pure . Left) (run F.. q)
			=<< either (run . h) (F.Pure . Right) =<< run m

{-# DEPRECATED runExc, runExcN "Don't use these" #-}

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
		Right (U.Fail m) -> Except.throwN nm $ err m
		Right (_ `U.FailCatch` _) -> error "bad" -- Except.catchN nm (runExcN nm err m) (runExcN nm err . h)

instance HFunctor.Tight U.Fail where
	mapT _ _ (U.Fail e) = U.Fail e
	mapT f _ (m `U.FailCatch` h) = (f m) `U.FailCatch` \e -> f $ h e

instance HFunctor.Loose U.Fail
