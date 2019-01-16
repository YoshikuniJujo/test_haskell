{-# LANGUAGE DataKinds, KindSignatures, TypeOperators, GADTs #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import GHC.TypeNats

infixr :+

data Tuple (n :: Nat) a where
	E :: Tuple 0 a
	(:+) :: a -> Tuple (n - 1) a -> Tuple n a

class ToList k where
	toList :: k a -> [a]

instance {-# OVERLAPPING #-} ToList (Tuple 0) where
	toList _ = []

instance ToList (Tuple (n - 1)) => ToList (Tuple n) where
	toList E = []
	toList (x :+ xs) = x : toList xs

data HasInt (n :: Nat) = HasInt (Tuple n Int)

data HasInts (n :: Nat) where
	HI :: HasInt n -> HasInts n
	NoMore :: HasInts (n - 1) -> HasInts n

class GetInts a where
	getInts :: a -> [Int]

instance {-# OVERLAPPING #-} GetInts (HasInts 0) where
	getInts _ = []

instance GetInts (HasInts (n - 1)) => GetInts (HasInts n) where
--	getInts (HI (HasInt is)) = toList is
	getInts (NoMore his) = getInts his
