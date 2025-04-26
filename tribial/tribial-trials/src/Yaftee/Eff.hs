{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Yaftee.Eff where

import Yaftee.HFreer qualified as HFreer
import Yaftee.OpenUnion qualified as Union

type E effs = HFreer.H (Union.U effs)

eff :: Union.Member (Union.FromFirst t) effs => t a -> E effs i o a
eff = (HFreer.:>>= HFreer.Pure) . Union.inj

effh :: Union.Member h effs => h (E effs) i o a -> E effs i o a
effh = (HFreer.:>>= HFreer.Pure) . Union.injh

runM :: Monad m => E '[Union.FromFirst m] i o a -> m a
runM (HFreer.Pure x) = pure x
runM (u HFreer.:>>= k) = runM . k =<< Union.extract u
