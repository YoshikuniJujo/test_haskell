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
import Control.Monad.HigherFreer qualified as HFreer
import Control.HigherOpenUnion qualified as Union
import Data.Functor.Identity
import Data.HigherFunctor qualified as HFunctor
import Data.FTCQueue qualified as Q

type F = Union.FromFirst Union.Fail

run :: HFunctor.Loose (Union.U effs) =>
	Eff.E (F ': effs) i o a -> Eff.E effs i o (Either String a)
run = \case
	HFreer.Pure x -> HFreer.Pure $ Right x
	u HFreer.:>>= q -> case Union.decomp u of
		Left u' -> HFunctor.map run Right u' HFreer.:>>= Q.singleton
			(either (HFreer.Pure . Left) (run `HFreer.comp` q))
		Right (Union.FromFirst (Union.Fail m) _) -> HFreer.Pure $ Left m

runExc :: (HFunctor.Loose (Union.U effs), Union.Member (Except.E e) effs) =>
	(String -> e) -> Eff.E (F ': effs) i o a -> Eff.E effs i o a
runExc = runExcN ""

runExcN :: forall e effs i o a . forall nm -> (
	HFunctor.Loose (Union.U effs),
	Union.Member (Except.Named nm e) effs ) =>
	(String -> e) -> Eff.E (F ': effs) i o a -> Eff.E effs i o a
runExcN nm err = \case
	HFreer.Pure x -> HFreer.Pure x
	u HFreer.:>>= q -> case Union.decomp u of
		Left u' -> HFunctor.map ((Identity <$>) . runExcN nm err)
				Identity u' HFreer.:>>=
			Q.singleton (runExcN nm err
					. (q `HFreer.app`) . runIdentity)
		Right (Union.FromFirst (Union.Fail m) _) ->
			Except.throwN nm $ err m
