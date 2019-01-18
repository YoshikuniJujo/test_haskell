{-# LANGUAGE TypeFamilies, GADTs #-}
{-# LANGUAGE DataKinds, KindSignatures, TypeOperators #-}
{-# LANGUAGE UndecidableInstances, FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gates (
	Gates, Id, Bit(..),
	mkAndGate, mkOrGate, mkNotGate ) where

import Data.Word

import GHC.TypeNats

data Bit = O | I deriving Show

andB, orB :: Bit -> Bit -> Bit
andB I I = I
andB _ _ = O
orB O O = O
orB _ _ = I

notB :: Bit -> Bit
notB O = I
notB I = O

data Tuple (n :: Nat) a where
	E :: Tuple 0 a
	(:+) :: a -> Tuple (n - 1) a -> Tuple n a

class ToList k where
	toList :: k a -> [a]

instance {-# OVERLAPPING #-} ToList (Tuple 0) where
	toList _ = []

instance {-# OVERLAPPABLE #-} ToList (Tuple (n - 1)) => ToList (Tuple n) where
	toList E = []
	toList (x :+ xs) = x : toList xs

infixr :+

{-
data family Tuple (n :: Nat) a where
	Tuple 0 a = E
	Tuple n a = a :+ Tuple (n - 1) a
-}

type Id = Word8

data Gate (i :: Nat) (o :: Nat) =
	Gate (Tuple i Id) (Tuple o Id) (Fun i o Bit Bit)

data Gates (i :: Nat) (o :: Nat) where
	Gates :: Gate i o -> Gates i o
--	MoreO :: Gates i o -> Gates i o
	NoMoreO :: Gates i (o - 1) -> Gates i o
	MoreI :: Gates i o -> Gates i o
	NoMoreI :: Gates (i - 1) o -> Gates i o

class IsGates a where
	getInputWire :: a -> [Id]

instance {-# OVERLAPPING #-} IsGates (Gates 0 o) where
	getInputWire _ = []

-- instance {-# OVERLAPPABLE #-} IsGates (Gates (i - 1) o) => IsGates (Gates 2 o) where
instance {-# OVERLAPPABLE #-} IsGates (Gates 2 o) where
	getInputWire (Gates (Gate is _ _)) = toList (is :: Tuple 2 Id)

{-
data family Gates (i :: Nat) (o :: Nat) where
	Gates 1 1 = Gates (Gate 1 1)
	Gates i o = MoreO (Gate i o) | NoMoreO (Gates i (o - 1))
	Gates i o = MoreI (Gate i o) | NoMoreI (Gates (i - 1) o)
-}

type family Fun (i :: Nat) (o :: Nat) a b where
	Fun 0 o a b = Tuple o b
	Fun i o a b = a -> Fun (i - 1) o a b

mkAndGate, mkOrGate :: Id -> Id -> Id -> Gates 2 2
mkAndGate i1 i2 o = MoreI . NoMoreO . Gates $
	Gate (i1 :+ i2 :+ E) (o :+ E) $ ((:+ E) .) . andB
mkOrGate i1 i2 o = MoreI . NoMoreO . Gates $
	Gate (i1 :+ i2 :+ E) (o :+ E) $ ((:+ E) .) . orB

mkNotGate :: Id -> Id -> Gates 2 2
mkNotGate i o = NoMoreI . NoMoreO . Gates $ Gate (i :+ E) (o :+ E) $ (:+ E) . notB
