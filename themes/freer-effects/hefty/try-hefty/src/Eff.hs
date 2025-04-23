{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Eff where

import Hefty qualified as Hefty
import OpenUnion qualified as Union

type E effs = Hefty.Hefty (Union.U effs)

eff :: Union.Member (Union.FromFirst t) effs => t a -> E effs a
eff = (Hefty.:>>= Hefty.Pure) . Union.inj

effh :: Union.Member h effs => h (E effs) a -> E effs a
effh = (Hefty.:>>= Hefty.Pure) . Union.injh

convertFromFirst (Union.FromFirst x) = Union.FromFirst x

runM :: Monad m => E '[(Union.FromFirst m)] a -> m a
runM (Hefty.Pure x) = pure x
runM (u Hefty.:>>= k) = runM . k =<< Union.extract u
