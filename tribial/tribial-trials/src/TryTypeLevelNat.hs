{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryTypeLevelNat where

import GHC.TypeNats
import GHC.Natural
import Data.Proxy

intToTypeNat_ :: Int -> (forall (n :: Nat) . Proxy n -> a) -> a
intToTypeNat_ 0 f = f (Proxy @0)
intToTypeNat_ n f = intToTypeNat_ (n - 1) \(_ :: Proxy m) -> f (Proxy @(m + 1))

natToTypeNat :: Natural -> (forall n . KnownNat n => Proxy n -> a) -> a
natToTypeNat nat f = (\(SomeNat n) -> f n) $ someNatVal nat

sample1 :: Natural
sample1 = natToTypeNat 15 natVal

sample2 :: (forall n . KnownNat n => Proxy n -> a) -> a
sample2 = natToTypeNat $ natVal (Proxy :: Proxy 15)
