{-# LANGUAGE GADTs, KindSignatures, DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TypeCheck.Test.TRange where

import GHC.TypeLits

import TypeCheck.Test.TFoldable

data TRangeL :: Nat -> Nat -> (* -> * -> *) -> * -> * -> * where
	NilL :: TRangeL 0 m c a a
	(:..) :: 1 <= m => c a x -> TRangeL 0 (m - 1) c x b -> TRangeL 0 m c a b
	(:.) :: c a x -> TRangeL (n - 1) (m - 1) c x b -> TRangeL n m c a b

instance TFoldable (TRangeL 0 0) where
	tfoldr _ z NilL = z
	tfoldr _ _ _ = error "never occur"
	tfoldl _ z NilL = z
	tfoldl _ _ _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	TFoldable (TRangeL 0 (m - 1)) => TFoldable (TRangeL 0 m) where
	tfoldr _ z NilL = z
	tfoldr (-<) z (x :.. xs) = x -< tfoldr (-<) z xs
	tfoldr _ _ _ = error "never occur"
	tfoldl _ z NilL = z
	tfoldl (>-) z (x :.. xs) = tfoldl (>-) (z >- x) xs
	tfoldl _ _ _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	TFoldable (TRangeL (n - 1) (m - 1)) => TFoldable (TRangeL n m) where
	tfoldr (-<) z (x :. xs) = x -< tfoldr (-<) z xs
	tfoldr _ _ _ = error "never occur"
	tfoldl (>-) z (x :. xs) = tfoldl (>-) (z >- x) xs
	tfoldl _ _ _ = error "never occur"
