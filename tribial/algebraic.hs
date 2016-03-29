{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.Coerce
import Test.QuickCheck
import System.Random

class Magma m where
	dot :: m -> m -> m

class (Magma q, Eq q) => Quasigroup q where
	tod :: q -> q -> q

prop_quasigroup :: Quasigroup q => q -> q -> Bool
prop_quasigroup x y =
	(x `dot` y) `tod` x == y &&
	(x `dot` y) `tod` y == x

class (Magma s, Eq s) => Semigroup s

prop_semigroup :: Semigroup s => s -> s -> s -> Bool
prop_semigroup x y z = (x `dot` y) `dot` z == x `dot` (y `dot` z)

instance Magma () where dot = const $ const ()
instance Semigroup ()

newtype And = And { getAnd :: Bool } deriving (Show, Eq, Arbitrary)

instance Magma And where dot = coerce (&&)
instance Semigroup And

newtype Sum a = Sum { getSum :: a } deriving (Show, Eq, Arbitrary)

instance Num a => Magma (Sum a) where Sum x `dot` Sum y = Sum $ x + y
instance (Num a, Eq a) => Semigroup (Sum a)
instance (Num a, Eq a) => Quasigroup (Sum a) where
	Sum z `tod` Sum x = Sum $ z - x

newtype Bad a = Bad { getBad :: a } deriving (Show, Eq, Arbitrary)

instance Num a => Magma (Bad a) where Bad x `dot` Bad y = Bad $ x - y
instance (Num a, Eq a) => Semigroup (Bad a)
