{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fplugin=Plugin.TypeCheck.Nat.Simple #-}

module Lib where

import GHC.TypeNats
import Data.Proxy

foo :: 1 <= n => Proxy (n - 1 + 1) -> Proxy (n + 1 - 1)
foo = id
