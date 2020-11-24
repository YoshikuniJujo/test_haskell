{-# LANGUAGE DataKinds, KindSignatures, TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances,
	UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.List.RangeL where

import GHC.TypeLits

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

class LoosenLMin n m n' where loosenLMin :: RangeL n m a -> RangeL n' m a

instance LoosenLMin 0 m 0 where
	loosenLMin NilL = NilL
	loosenLMin xa@(_ :.. _) = xa
	loosenLMin _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	(1 <= m, LoosenLMin (n - 1) (m - 1) 0) => LoosenLMin n m 0 where
	loosenLMin (x :. xs) = x :.. loosenLMin xs
	loosenLMin _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	LoosenLMin (n - 1) (m - 1) (n' - 1) => LoosenLMin n m n' where
	loosenLMin (x :. xs) = x :. loosenLMin xs
	loosenLMin _ = error "never occur"

class LoosenLMax n m m' where loosenLMax :: RangeL n m a -> RangeL n m' a

instance LoosenLMax 0 0 m where
	loosenLMax NilL = NilL
	loosenLMax _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	(1 <= m', LoosenLMax 0 (m - 1) (m' - 1)) => LoosenLMax 0 m m' where
	loosenLMax NilL = NilL
	loosenLMax (x :.. xs) = x :.. loosenLMax xs
	loosenLMax _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	LoosenLMax (n - 1) (m - 1) (m' - 1) => LoosenLMax n m m' where
	loosenLMax (x :. xs) = x :. loosenLMax xs
	loosenLMax _ = error "never occur"

loosenL :: (LoosenLMin n m n', LoosenLMax n' m m') => RangeL n m a -> RangeL n' m' a
loosenL = loosenLMax . loosenLMin
