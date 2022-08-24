{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryTypeLevelNat where

import GHC.TypeNats
import GHC.Natural

data Proxy (n :: Nat) = Proxy

intToTypeNat :: Int -> (forall n . Proxy n -> a) -> a
intToTypeNat 0 f = f (Proxy @0)
intToTypeNat n f = intToTypeNat (n - 1) \(_ :: Proxy m) -> f (Proxy @(m + 1))

-- sample :: Natural
-- sample = intToTypeNat 15 natVal
