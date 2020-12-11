{-# LANGUAGE DataKinds, TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Data.Proxy

import GHC.TypeNats

foo :: Proxy ((n + 1) - 1) -> Proxy ((n - 1) + 1)
foo = id
