{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Yaftee.UseFTCQ.Eff where

import Yaftee.UseFTCQ.HFreer qualified as HFreer
import Yaftee.OpenUnion qualified as Union

import Data.FTCQueue qualified as Q

type E effs = HFreer.H (Union.U effs)

eff :: Union.Member (Union.FromFirst t) effs => t a -> E effs i o a
eff = (HFreer.:>>= Q.singleton HFreer.Pure) . Union.inj

effBase :: Union.Base (Union.FromFirst t) effs => t a -> E effs i o a
effBase = (HFreer.:>>= Q.singleton HFreer.Pure) . Union.injBase

effh :: Union.Member h effs => h (E effs) i o a -> E effs i o a
effh = (HFreer.:>>= Q.singleton HFreer.Pure) . Union.injh

run :: E '[] i o a -> a
run (HFreer.Pure x) = x
run _ = error "bad"

runM :: Monad m => E '[Union.FromFirst m] i o a -> m a
runM (HFreer.Pure x) = pure x
runM (u HFreer.:>>= q) = runM . (q `HFreer.app`) =<< Union.extract u
