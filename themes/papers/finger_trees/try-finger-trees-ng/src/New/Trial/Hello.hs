{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fplugin=New.TypeCheck.Nat #-}

module New.Trial.Hello where

import GHC.TypeLits
import Data.Proxy

{-
some :: Proxy (m + 1) -> Proxy (n - 1)
some = id

other :: Proxy True -> Proxy False
other = id

boo :: 1 <= m => Proxy m
boo = Proxy

foo :: Proxy (n :: Nat)
foo = boo
-}
