{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

{-# LANGUAGE TypeOperators #-}

module Control.Monad.Yaftee.Trace (T, trace, run, ignore) where

import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.HigherFreer qualified as HFreer
import Control.HigherOpenUnion qualified as Union

import Control.Monad.HigherFreer qualified as F
import Data.HigherFunctor qualified as HFunctor
import Data.FTCQueue qualified as Q
import Data.Functor.Identity

type T = Union.FromFirst T_
data T_ a where T_ :: String -> T_ ()

trace :: Union.Member T effs => String -> Eff.E effs i o ()
trace = Eff.eff . T_

run :: Eff.E '[T] i o a -> IO a
run = \case
	HFreer.Pure x -> pure x
	u HFreer.:>>= q -> case Union.extracth u of
		Union.FromFirst (T_ s) k -> putStrLn s >> run (q HFreer.$ k ())

ignore ::
	HFunctor.Loose (Union.U es) =>
	Eff.E (T ': es) i o r -> Eff.E es i o r
ignore = \case
	HFreer.Pure x -> HFreer.Pure x
	u F.:>>= q -> case Union.decomp u of
		Left u' -> HFunctor.map ((Identity <$>) . ignore) Identity u'
			F.:>>= Q.singleton ((ignore F.. q) . runIdentity)
		Right (Union.FromFirst (T_ _) k) -> ignore HFreer.. q $ k ()
