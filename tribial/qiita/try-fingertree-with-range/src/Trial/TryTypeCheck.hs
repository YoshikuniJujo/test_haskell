{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fplugin=Plugin.TypeCheck.Nat.Simple #-}

module Trial.TryTypeCheck where

import GHC.TypeNats
import Data.Proxy

bar :: a <= b => Proxy a -> Proxy b
bar = id
