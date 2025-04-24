{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ExplicitForAll, RequiredTypeArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Except (

	-- * NORMAL

	E, throw, catch, run,

	-- * NAMED

	Named, throwN, catchN, runN
	
	) where

import GHC.TypeLits
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.HFreer qualified as HFreer
import Control.HigherOpenUnion qualified as Union
import Data.HFunctor qualified as HFunctor
import Data.FTCQueue qualified as Q

-- * NORMAL

type E = Named ""

throw :: Union.Member (E e) effs => e -> Eff.E effs a
throw = throwN ""

catch :: Union.Member (E e) effs =>
	Eff.E effs a -> (e -> Eff.E effs a) -> Eff.E effs a
catch = catchN ""

run :: HFunctor.H (Union.U effs) =>
	Eff.E (E e ': effs) a -> Eff.E effs (Either e a)
run = runN

-- * NAMED

data Named (nm :: Symbol) e f a where
	Throw :: forall nm e f a . e -> Named nm e f a
	Catch :: forall nm e f a . f a -> (e -> f a) -> Named nm e f a

throwN :: forall nm -> Union.Member (Named nm e) effs => e -> Eff.E effs a
throwN nm = Eff.effh . Throw @nm

catchN :: forall nm -> Union.Member (Named nm e) effs =>
	Eff.E effs a -> (e -> Eff.E effs a) -> Eff.E effs a
catchN nm = (Eff.effh .) . Catch @nm

runN :: HFunctor.H (Union.U effs) =>
	Eff.E (Named nm e ': effs) a -> Eff.E effs (Either e a)
runN = \case
	HFreer.Pure x -> HFreer.Pure $ Right x
	u HFreer.:>>= q -> case Union.decomp u of
		Left u' -> HFunctor.map runN Right u' HFreer.:>>= Q.singleton
			(either (HFreer.Pure . Left) (runN `HFreer.comp` q))
		Right (Throw e) -> HFreer.Pure $ Left e
		Right (m `Catch` h) ->
			either (HFreer.Pure . Left) (runN `HFreer.comp` q)
				=<< either (runN . h) (HFreer.Pure . Right)
				=<< runN m

instance HFunctor.H (Named nm e) where
	map _ _ (Throw e) = Throw e
	map f _ (m `Catch` h) = (f m) `Catch` \e -> f $ h e
