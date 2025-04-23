{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module UseFTCQ.Eff where

import UseFTCQ.HFreer qualified as HFreer
import Data.FTCQueue qualified as Q
import OpenUnion qualified as Union

type E effs = HFreer.H (Union.U effs)

eff :: Union.Member (Union.FromFirst t) effs => t a -> E effs a
eff = (HFreer.:>>= Q.singleton HFreer.Pure) . Union.inj

effh :: Union.Member h effs => h (E effs) a -> E effs a
effh = (HFreer.:>>= Q.singleton HFreer.Pure) . Union.injh

run :: E '[] a -> a
run (HFreer.Pure x) = x
run _ = error "Eff.run: This function can run only Pure"

runM :: Monad m => E '[(Union.FromFirst m)] a -> m a
runM (HFreer.Pure x) = pure x
runM (u HFreer.:>>= q) = runM . (q `HFreer.app`) =<< Union.extract u
