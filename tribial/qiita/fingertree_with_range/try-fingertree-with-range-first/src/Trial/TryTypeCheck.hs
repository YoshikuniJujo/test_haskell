{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fplugin=Plugin.TypeCheck.Nat.Simple #-}

module Trial.TryTypeCheck where

import GHC.TypeNats
import Data.Proxy

-- foo :: Proxy hoge -> Proxy piyo
-- foo = id

bar :: (a + 1) ~ (b + 1) => Proxy a -> Proxy b
bar = id
