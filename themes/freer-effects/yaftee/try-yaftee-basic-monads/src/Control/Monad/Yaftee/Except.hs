{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE ExplicitForAll, RequiredTypeArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Except (

	-- * NORMAL

	E, throw, catch, run, runExc, runIO,

	-- * NAMED

	Named, throwN, catchN, runN, runExcN, runION
	
	) where

import GHC.TypeLits
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.IO qualified as IO
import Control.Monad.HFreer qualified as HFreer
import Control.HigherOpenUnion qualified as Union
import Control.Exception qualified as IO
import Data.HFunctor qualified as HFunctor
import Data.FTCQueue qualified as Q

-- * NORMAL

type E = Named ""

throw :: Union.Member (E e) effs => e -> Eff.E effs a
throw = throwN ""

catch :: Union.Member (E e) effs =>
	Eff.E effs a -> (e -> Eff.E effs a) -> Eff.E effs a
catch = catchN ""

run :: forall e effs a . HFunctor.H (Union.U effs) =>
	Eff.E (E e ': effs) a -> Eff.E effs (Either e a)
run = runN

runExc :: forall e e' effs a .
	(HFunctor.H (Union.U effs), Union.Member (E e') effs) =>
	(e -> e') -> (e' -> e) -> Eff.E (E e ': effs) a -> Eff.E effs a
runExc = runExcN ""

runIO :: IO.Exception e => Eff.E '[E e, IO.I] a -> Eff.E '[IO.I] a
runIO = runION

-- * NAMED

data Named (nm :: Symbol) e f a where
	Throw :: forall nm e f a . e -> Named nm e f a
	Catch :: forall nm e f a . f a -> (e -> f a) -> Named nm e f a

instance HFunctor.H (Named nm e) where
	map _ _ (Throw e) = Throw e
	map f _ (m `Catch` h) = (f m) `Catch` \e -> f $ h e

throwN :: forall nm -> Union.Member (Named nm e) effs => e -> Eff.E effs a
throwN nm = Eff.effh . Throw @nm

catchN :: forall nm -> Union.Member (Named nm e) effs =>
	Eff.E effs a -> (e -> Eff.E effs a) -> Eff.E effs a
catchN nm = (Eff.effh .) . Catch @nm

runN :: forall nm e effs a . HFunctor.H (Union.U effs) =>
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

runExcN :: forall nm e e' effs a . forall nm' -> (
	HFunctor.H (Union.U effs),
	Union.Member (Named nm' e') effs
	) =>
	(e -> e') -> (e' -> e) -> Eff.E (Named nm e ': effs) a -> Eff.E effs a
runExcN nm' c c' = \case
	HFreer.Pure x -> HFreer.Pure x
	u HFreer.:>>= q -> case Union.decomp u of
		Left u' -> HFunctor.map (runExcN nm' c c') id u' HFreer.:>>=
			Q.singleton (runExcN nm' c c' `HFreer.comp` q)
		Right (Throw e) -> throwN nm' $ c e
		Right (m `Catch` h) -> runExcN nm' c c' `HFreer.comp` q
			=<< catchN nm'
				(runExcN nm' c c' m) (runExcN nm' c c' . h . c')

runION :: IO.Exception e => Eff.E '[Named nm e, IO.I] a -> Eff.E '[IO.I] a
runION = \case
	HFreer.Pure x -> HFreer.Pure x
	u HFreer.:>>= q -> case Union.decomp u of
		Left u' -> HFunctor.map runION id u' HFreer.:>>=
			Q.singleton (runION `HFreer.comp` q)
		Right (Throw e) -> Eff.effBase $ IO.throwIO e
		Right (m `Catch` h) ->
			runION `HFreer.comp` q =<< runION m `cch` (runION . h)
	where m `cch` h = Eff.effBase $ Eff.runM m `IO.catch` (Eff.runM . h)
