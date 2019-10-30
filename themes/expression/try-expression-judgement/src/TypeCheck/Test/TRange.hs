{-# LANGUAGE GADTs, KindSignatures, DataKinds, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fplugin=TypeCheck.Nat #-}

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

infixr 5 .:..

class PushL n m where
	(.:..) :: c x y -> TRangeL n m c y z -> TRangeL n (m + 1) c x z

instance 1 <= m + 1 => PushL 0 m where
	(.:..) = (:..)

instance {-# OVERLAPPABLE #-} PushL (n - 1) (m - 1) => PushL n m where
	x .:.. (y :. ys) = x :. (y .:.. ys)
	_ .:.. _ = error "never occur"

class LoosenLMin n m n' where
	loosenLMin :: TRangeL n m c x y -> TRangeL n' m c x y

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
