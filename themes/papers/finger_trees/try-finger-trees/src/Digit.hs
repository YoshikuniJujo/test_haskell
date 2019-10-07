{-# LANGUAGE GADTs, DataKinds, KindSignatures, TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Digit where

import GHC.TypeLits

infixr 5 :., :..

data Digit :: Nat -> Nat -> * -> * where
	Nil :: Digit 0 0 a
	Gap :: Digit 0 (m - 1) a -> Digit 0 m a
	(:..) :: a -> Digit 0 (m - 1) a -> Digit 0 m a
	(:.) :: a -> Digit (n - 1) (m - 1) a -> Digit n m a

deriving instance Show a => Show (Digit n m a)

type Digit14 = Digit 1 4
type Node = Digit 2 3

instance Foldable (Digit 0 0) where
	foldr _ z Nil = z
	foldr _ _ _ = error "never occur"
	foldl _ z Nil = z
	foldl _ _ _ = error "never occur"

instance {-# OVERLAPPABLE #-} Foldable (Digit 0 (m - 1)) => Foldable (Digit 0 m) where
	foldr (-<) z (Gap xs) = foldr (-<) z xs
	foldr (-<) z (x :.. xs) = x -< foldr (-<) z xs
	foldr _ _ _ = error "never occur"
	foldl (>-) z (Gap xs) = foldl (>-) z xs
	foldl (>-) z (x :.. xs) = foldl (>-) (z >- x) xs
	foldl _ _ _ = error "never occur"

instance {-# OVERLAPPABLE #-} Foldable (Digit (n - 1) (m - 1)) => Foldable (Digit n m) where
	foldr (-<) z (x :. xs) = x -< foldr (-<) z xs
	foldr _ _ _ = error "never occur"
	foldl (>-) z (x :. xs) = foldl (>-) (z >- x) xs
	foldl _ _ _ = error "never occur"
