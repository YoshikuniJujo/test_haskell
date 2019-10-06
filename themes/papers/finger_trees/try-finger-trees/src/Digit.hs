{-# LANGUAGE GADTs, DataKinds, KindSignatures, StandaloneDeriving, TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Digit where

import GHC.TypeLits

infixr 5 :., :..

data Digit :: Nat -> Nat -> * -> * where
	Nil :: Digit 0 0 a
	Cut :: Digit 0 (m - 1) a -> Digit 0 m a
	(:..) :: a -> Digit 0 (m - 1) a -> Digit 0 m a
	(:.) :: a -> Digit (n - 1) (m - 1) a -> Digit n m a

deriving instance Show a => Show (Digit n m a)

type Digit14 = Digit 1 4
type Node = Digit 2 3
