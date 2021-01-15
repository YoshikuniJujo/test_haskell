{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, RankNTypes #-}
{-# LANGUAGE DataKinds, KindSignatures, TypeOperators #-}
{-# LANGUAGE TypeFamilies, ExistentialQuantification #-}
{-# LANGUAGE
	MultiParamTypeClasses,
 	FlexibleContexts, FlexibleInstances,
	UndecidableInstances, AllowAmbiguousTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
-- {-# OPTIONS_GHC -Wall -fno-warn-tabs -fplugin=TypeCheck #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

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

-- fib'' :: Int -> (forall (n :: Nat) . KnownNat n => Proxy n -> x) -> x
fib'' :: Int -> (forall (n :: Nat) . Proxy n -> x) -> x
fib'' 0 f = f $ Proxy @0
fib'' 1 f = f $ Proxy @1
fib'' i f = fib'' (i - 2) \p -> fib'' (i - 1) \p' -> f $ p `add` p'

add :: Proxy n -> Proxy m -> Proxy (n + m)
add _ _ = Proxy

data MixNat = forall (n :: Nat) . MixNat (Proxy n)

deriving instance Show MixNat

data AddNat = Zero | One | AddNat :+ AddNat deriving Show

afib :: Int -> (forall (n :: AddNat) . KnownAddNat n => Proxy n -> x) -> x
afib 0 f = f $ Proxy @'Zero
afib 1 f = f $ Proxy @'One
afib i f = afib (i - 2) \p -> afib (i - 1) \p' -> f $ p `add'` p'

add' :: Proxy n -> Proxy m -> Proxy (n ':+ m)
add' _ _ = Proxy

class KnownAddNat (n :: AddNat) where addNatVal :: Proxy n -> AddNat


instance KnownAddNat 'Zero where addNatVal _ = Zero
instance KnownAddNat 'One where addNatVal _ = One
instance {-# OVERLAPPABLE #-} (KnownAddNat n, KnownAddNat m) => KnownAddNat (n ':+ m) where
	addNatVal _ = addNatVal (Proxy @n) :+ addNatVal (Proxy @m)

toNatural :: AddNat -> Natural
toNatural Zero = 0
toNatural One = 1
toNatural (m :+ n) = toNatural m + toNatural n

---------------------------------------------------------------------------

afib' :: Int -> AddNat
afib' 0 = Zero
afib' 1 = One
afib' i = afib' (i - 2) :+ afib' (i - 1)

test :: Int -> (Integer -> x) -> x
test 0 f = f 0
test 1 f = f 1
test i f = test (i - 1) \n -> test (i - 2) \n' -> f $ n + n'
