{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

type N = Union.FromFirst Union.NonDet

run :: forall f effs i o a .
	(Union.HFunctor (Union.U effs), Traversable f, MonadPlus f) =>
	Eff.E (N ': effs) i o a -> Eff.E effs i o (f a)
run = \case
	F.Pure x -> F.Pure $ pure x
	u F.:>>= q -> case Union.decomp u of
		Left u' -> Union.hmap run pure u' F.:>>=
			Q.singleton ((join <$>) . ((run . (q `F.app`)) `traverse`))
		Right (Union.FromFirst Union.MZero _) -> pure empty
		Right (Union.FromFirst Union.MPlus k) ->
			(<|>) <$> run (q `F.app` k False) <*> run (q `F.app` k True)
