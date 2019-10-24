{-# LANGUAGE ScopedTypeVariables, InstanceSigs #-}
{-# LANGUAGE GADTs, DataKinds, TypeOperators, KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fplugin=TypeCheck.Nat #-}

module TypeCheck.Test.Range (RangeL(..), PushL(..), RangeR(..), PushR(..)) where

import GHC.TypeLits

--------------------------------------------------------------------------------
-- RangeL
--------------------------------------------------------------------------------

infixr 6 :., :..

data RangeL :: Nat -> Nat -> * -> * where
	NilL :: RangeL 0 m a
	(:..) :: 1 <= m => a -> RangeL 0 (m - 1) a -> RangeL 0 m a
	(:.) :: a -> RangeL (n - 1) (m - 1) a -> RangeL n m a

deriving instance Show a => Show (RangeL n m a)

instance Foldable (RangeL 0 0) where
	foldr _ z NilL = z
	foldr _ _ _ = error "never occur"
	foldl _ z NilL = z
	foldl _ _ _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	Foldable (RangeL 0 (m - 1)) => Foldable (RangeL 0 m) where
	foldr _ z NilL = z
	foldr (-<) z (x :.. xs) = x -< foldr (-<) z xs
	foldr _ _ _ = error "never occur"
	foldl _ z NilL = z
	foldl (>-) z (x :.. xs) = foldl (>-) (z >- x) xs
	foldl _ _ _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	Foldable (RangeL (n - 1) (m - 1)) => Foldable (RangeL n m) where
	foldr (-<) z (x :. xs) = x -< foldr (-<) z xs
	foldr _ _ _ = error "never occur"
	foldl (>-) z (x :. xs) = foldl (>-) (z >- x) xs
	foldl _ _ _ = error "never occur"

infixr 5 .:..

class PushL n m where
	(.:..) :: a -> RangeL n m a -> RangeL n (m + 1) a

instance 1 <= m + 1 => PushL 0 m where
	(.:..) = (:..)

instance {-# OVERLAPPABLE #-} (1 <= m + 1, PushL (n - 1) (m - 1)) => PushL n m where
	x .:.. (y :. ys) = x :. (y .:.. ys)
	_ .:.. _ = error "never occur"

--------------------------------------------------------------------------------
-- RangeR
--------------------------------------------------------------------------------

infixl 6 :+, :++

data RangeR :: Nat -> Nat -> * -> * where
	NilR :: RangeR 0 m a
	(:++) :: 1 <= m => RangeR 0 (m - 1) a -> a -> RangeR 0 m a
	(:+) :: RangeR (n - 1) (m - 1) a -> a -> RangeR n m a

deriving instance Show a => Show (RangeR n m a)

instance Foldable (RangeR 0 0) where
	foldr _ z NilR = z
	foldr _ _ _ = error "never occur"
	foldl _ z NilR = z
	foldl _ _ _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	Foldable (RangeR 0 (m - 1)) => Foldable (RangeR 0 m) where
	foldr _ z NilR = z
	foldr (-<) z (xs :++ x) = foldr (-<) (x -< z) xs
	foldr _ _ _ = error "never occur"
	foldl _ z NilR = z
	foldl (>-) z (xs :++ x) = foldl (>-) z xs >- x
	foldl _ _ _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	Foldable (RangeR (n - 1) (m - 1)) => Foldable (RangeR n m) where
	foldr (-<) z (xs :+ x) = foldr (-<) (x -< z) xs
	foldr _ _ _ = error "never occur"
	foldl (>-) z (xs :+ x) = foldl (>-) z xs >- x
	foldl _ _ _ = error "never occur"

infixl 5 .:++

class PushR n m where
	(.:++) :: RangeR n m a -> a -> RangeR n (m + 1) a

instance 1 <= m + 1 => PushR 0 m where
	(.:++) = (:++)

instance {-# OVERLAPPABLE #-} (1 <= m + 1, PushR (n - 1) (m - 1)) => PushR n m where
	(xs :+ x) .:++ y = (xs .:++ x) :+ y
	_ .:++ _ = error "never occur"
