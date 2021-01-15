{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, RankNTypes #-}
{-# LANGUAGE DataKinds, KindSignatures, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE
	MultiParamTypeClasses,
 	FlexibleContexts, FlexibleInstances,
	UndecidableInstances, AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fplugin=TypeCheck #-}

module Simple where

import GHC.TypeNats
import GHC.Natural
import Data.Proxy

fib :: Int -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 2) + fib (n - 1)

class Fib (n :: Nat) where fib' :: Integer

instance Fib 0 where fib' = 0
instance Fib 1 where fib' = 1
instance {-# OVERLAPPABLE #-} (Fib (n - 2), Fib (n - 1)) => Fib n where
	fib' = fib' @(n - 2) + fib' @(n - 1)

type family Fib' n where
	Fib' 0 = 0
	Fib' 1 = 1
	Fib' n = Fib' (n - 2) + Fib' (n - 1)

valFib' :: forall (n :: Nat) . KnownNat (Fib' n) => Natural
valFib' = natVal $ Proxy @(Fib' n)

fib'' :: Int -> (forall (n :: Nat) . KnownNat n => Proxy n -> x) -> x
-- fib'' :: Int -> (forall (n :: Nat) . Proxy n -> x) -> x
fib'' 0 f = f $ Proxy @0
fib'' 1 f = f $ Proxy @1
fib'' i f = fib'' (i - 2) \p -> fib'' (i - 1) \p' -> f $ p `add` p'

-- add :: KnownNat (n + m) => Proxy n -> Proxy m -> Proxy (n + m)
add :: Proxy n -> Proxy m -> Proxy (n + m)
add _ _ = Proxy

test :: Int -> (Integer -> x) -> x
test 0 f = f 0
test 1 f = f 1
test i f = test (i - 1) \n -> test (i - 2) \n' -> f $ n + n'
