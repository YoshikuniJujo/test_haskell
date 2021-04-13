{-# LANGUAGE DataKinds, KindSignatures, TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Data.Proxy

import GHC.TypeNats

foo :: ((m + 1) - 1) ~ ((m - 1) + 1) => Proxy ((m + 1) - 1) -> Proxy ((m - 1) + 1)
foo = id

bar :: Proxy (n :: Nat) -> Proxy n
bar = foo
