{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fplugin=Plugin.TypeCheck.Nat.Simple #-}

module Trial.TryTypeCheck where

import GHC.TypeNats
import Data.Proxy

-- foo :: Proxy (a + b) -> Proxy ((a + 1) + (b - 1))
-- foo :: Proxy (a + b + 1 - 1) -> Proxy (a + b - 1 + 1)
foo :: Proxy (a + b) -> Proxy ((a + 1) + (b - 1))
foo = id
