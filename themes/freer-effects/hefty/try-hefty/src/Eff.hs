{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE FlexibleContexts #-}

module Eff where

import Hefty qualified as Hefty
import OpenUnion qualified as Union

type E effs = Hefty.Hefty (Union.U effs)

eff :: Union.Member (Union.FromFirst t) effs => t a -> E effs a
eff = (Hefty.:>>= Hefty.Pure) . Union.inj

convertFromFirst (Union.FromFirst x) = Union.FromFirst x
