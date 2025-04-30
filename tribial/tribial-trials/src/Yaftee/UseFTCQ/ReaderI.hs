{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Yaftee.UseFTCQ.ReaderI where

import Control.Monad.Fix
import Data.Functor.Identity
import Data.Kind
import Yaftee.UseFTCQ.Eff qualified as Eff
import Yaftee.UseFTCQ.HFreer qualified as HFreer
import Yaftee.OpenUnion qualified as Union
import Yaftee.HFunctor qualified as HFunctor
import Data.FTCQueue qualified as Q

data R (f :: Type -> Type -> Type -> Type) i o a where
	Ask :: (i -> a) -> R f i o a

instance HFunctor.HFunctor R where
	hmap _ g (Ask k) = Ask $ g . k

-- instance HFunctor.HFunctor' R where
--	hmap' _ g (Ask k) = Ask $ g . k

ask :: Union.Member R effs => Eff.E effs i o i
ask = Eff.effh $ Ask id

run :: HFunctor.HFunctor' (Union.U effs) =>
	Eff.E (R ': effs) i o a -> i -> Eff.E effs i o (Identity a)
m `run` i = ($ m) $ fix \go -> \case
	HFreer.Pure x -> HFreer.Pure $ Identity x
	u HFreer.:>>= q -> case Union.decomp u of
		Left u' -> HFunctor.hmap' (`run` i) Identity u' HFreer.:>>=
			Q.singleton \x -> go $ q `HFreer.app` runIdentity x
		Right (Ask k) -> go `HFreer.comp` q $ k i
