{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.NonDet (N, run) where

import Control.Applicative
import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.HigherFreer qualified as HFreer
import Control.HigherOpenUnion qualified as Union
import Data.HigherFunctor qualified as HFunctor
import Data.FTCQueue qualified as Q

type N = Union.FromFirst Union.NonDet

run :: forall f effs i o a .
	(HFunctor.Loose (Union.U effs), Traversable f, MonadPlus f) =>
	Eff.E (N ': effs) i o a -> Eff.E effs i o (f a)
run = \case
	HFreer.Pure x -> HFreer.Pure $ pure x
	u HFreer.:>>= q -> case Union.decomp u of
		Left u' -> HFunctor.map run pure u' HFreer.:>>=
			Q.singleton \xs ->
				join <$> (run . (q `HFreer.app`)) `traverse` xs
		Right (Union.FromFirst Union.MZero _) -> pure empty
		Right (Union.FromFirst Union.MPlus k) -> (<|>)
			<$> run (q `HFreer.app` k False)
			<*> run (q `HFreer.app` k True)
