{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, RankNTypes #-}
{-# LANGUAGE DataKinds, KindSignatures, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE
	FlexibleContexts, FlexibleInstances, UndecidableInstances,
	AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Fib where

import GHC.TypeNats
import GHC.Natural
import Data.Proxy

-- VALUE -> VALUE

fib :: Int -> FibNat
fib 0 = Zero
fib 1 = One
fib n = fib (n - 2) :+ fib (n - 1)

-- > toNatural $ fib 20 ===> 6765

-- TYPE -> VALUE

class FibC (n :: Nat) where fib' :: FibNat

instance FibC 0 where fib' = Zero
instance FibC 1 where fib' = One

instance {-# OVERLAPPABLE #-} (FibC (n - 2), FibC (n - 1)) => FibC n where
	fib' = fib' @(n - 2) :+ fib' @(n - 1)

-- > toNatural $ fib' @20 ===> 6765

-- TYPE -> TYPE

type family Fib n where
	Fib 0 = 'Zero
	Fib 1 = 'One
	Fib n = Fib (n - 2) ':+ Fib (n - 1)

-- > toNatural . fibNatVal $ Proxy @(Fib 20) ===> 6765

-- VALUE -> TYPE

fib'' :: Int -> (forall (n :: FibNat) . KnownFibNat n => Proxy n -> x) -> x
fib'' 0 f = f $ Proxy @'Zero
fib'' 1 f = f $ Proxy @'One
fib'' i f = fib'' (i - 2) \p -> fib'' (i - 1) \p' -> f $ p .+ p'
	where
	(.+) :: Proxy n -> Proxy m -> Proxy (n ':+ m)
	_ .+ _ = Proxy

-- > toNatural $ fib'' 20 fibNatVal ===> 6765

---------------------------------------------------------------------------

data FibNat = Zero | One | FibNat :+ FibNat deriving Show

toNatural :: FibNat -> Natural
toNatural Zero = 0
toNatural One = 1
toNatural (n :+ m) = toNatural n + toNatural m

class KnownFibNat (n :: FibNat) where fibNatVal :: Proxy n -> FibNat

instance KnownFibNat 'Zero where fibNatVal _ = Zero
instance KnownFibNat 'One where fibNatVal _ = One

instance {-# OVERLAPPABLE #-}
	(KnownFibNat n, KnownFibNat m) => KnownFibNat (n ':+ m) where
	fibNatVal _ = fibNatVal (Proxy @n) :+ fibNatVal (Proxy @m)
