{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fplugin=TypeCheck.Nat #-}

module Trial.TypeCheck.Hello where

import GHC.TypeLits
import Data.Proxy

plusOne :: (m + 1) ~ (n + 1) => Proxy m -> Proxy n
plusOne = id

foo :: (moocho - 1) ~ (noocho - 1) => Proxy moocho -> Proxy noocho
foo = id

bar :: Proxy 0 -> Proxy 0
bar = foo

{-
ohmygosh :: ((m + 1) + (m' - 1)) ~ (m + m') => a -> a
ohmygosh = id
-}

-- plusOneBad :: (m + 1) ~ (n + 1) => Proxy (m - 1) -> Proxy n
-- plusOneBad = id

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
