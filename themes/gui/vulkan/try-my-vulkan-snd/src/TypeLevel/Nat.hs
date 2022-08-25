{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TypeLevel.Nat (Nat, type (-), valNat, natVal) where

import GHC.TypeNats
import Data.Proxy

valNat :: Natural -> (forall n . KnownNat n => Proxy n -> a) -> a
valNat nat f = (\(SomeNat n) -> f n) $ someNatVal nat
