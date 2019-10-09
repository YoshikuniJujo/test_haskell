-- {-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs, DataKinds, KindSignatures, TypeOperators, RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fplugin=GHC.TypeLits.Normalise #-}

module TRange where

import GHC.TypeLits

import TFoldable

infixr 6 :., :..

data TRange :: Nat -> Nat -> (* -> * -> *) -> * -> * -> * where
	Nil :: TRange 0 m c a a
	(:..) :: 1 <= m => c a x -> TRange 0 (m - 1) c x b -> TRange 0 m c a b
	(:.) :: c a x -> TRange (n - 1) (m - 1) c x b -> TRange n m c a b

instance TFoldable (TRange 0 0) where
	tfoldr _ z Nil = z
	tfoldr _ _ _ = error "never occur"
	tfoldl _ z Nil = z
	tfoldl _ _ _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	TFoldable (TRange 0 (m - 1)) => TFoldable (TRange 0 m) where
	tfoldr _ z Nil = z
	tfoldr (-<) z (x :.. xs) = x -< tfoldr (-<) z xs
	tfoldr _ _ _ = error "never occur"
	tfoldl _ z Nil = z
	tfoldl (>-) z (x :.. xs) = tfoldl (>-) (z >- x) xs
	tfoldl _ _ _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	TFoldable (TRange (n - 1) (m - 1)) => TFoldable (TRange n m) where
	tfoldr (-<) z (x :. xs) = x -< tfoldr (-<) z xs
	tfoldr _ _ _ = error "never occur"
	tfoldl (>-) z (x :. xs) = tfoldl (>-) (z >- x) xs
	tfoldl _ _ _ = error "never occur"

class TLoosen s t where
	tloosen :: s (c :: * -> * -> *) x y -> t c x y

instance TLoosen (TRange 0 0) (TRange 0 0) where
	tloosen = id

instance {-# OVERLAPPABLE #-} TLoosen (TRange 0 0) (TRange 0 (m' - 1)) =>
	TLoosen (TRange 0 0) (TRange 0 m') where
	tloosen Nil = Nil
	tloosen _ = error "never occur"

instance {-# OVERLAPPABLE #-} (1 <= m', TLoosen (TRange 0 (m - 1)) (TRange 0 (m' - 1))) =>
	TLoosen (TRange 0 m) (TRange 0 m') where
	tloosen Nil = Nil
	tloosen (x :.. xs) = x :.. tloosen xs
	tloosen _ = error "never occur"

instance {-# OVERLAPPABLE #-} (1 <= m', TLoosen (TRange (n - 1) (m - 1)) (TRange 0 (m' - 1))) =>
	TLoosen (TRange n m) (TRange 0 m') where
	tloosen (x :. xs) = x :.. tloosen xs
	tloosen _ = error "never occur"

instance {-# OVERLAPPABLE #-} TLoosen (TRange (n - 1) (m - 1)) (TRange (n' - 1) (m' - 1)) =>
	TLoosen (TRange n m) (TRange n' m') where
	tloosen (x :. xs) = x :. tloosen xs
	tloosen _ = error "never occur"

infix 5 :|

data TPair :: (* -> * -> *) -> (* -> * -> *) -> * -> * -> * where
	(:|) :: c x y -> d y z -> TPair c d x z

firstTP :: (forall y . c x y -> c' x' y) -> TPair c d x z -> TPair c' d x' z
firstTP f (c :| d) = f c :| d

class Unsnoc n m where
	unsnoc :: TRange n m c x y -> TPair (TRange (n - 1) (m - 1) c) c x y

instance Unsnoc 1 1 where
	unsnoc (x :. Nil) = Nil :| x
	unsnoc _ = error "never occur"

instance {-# OVERLAPPABLE #-} Unsnoc 1 (m - 1) => Unsnoc 1 m where
	unsnoc (x :. Nil) = Nil :| x
	unsnoc (x :. y :.. xs) = (x :..) `firstTP` unsnoc (y :. xs)
	unsnoc _ = error "never occur"

instance {-# OVERLAPPABLE #-} Unsnoc (n - 1) (m - 1) => Unsnoc n m where
	unsnoc (x :. xs) = (x :.) `firstTP` unsnoc xs
	unsnoc _ = error "never occur"

infixr 5 ++.., ++.

(++..) :: 1 <= m + m' => TRange 0 m c x y -> TRange n' m' c y z -> TRange n' (m + m') c x z
Nil ++.. ys = loosenMax ys
x :.. xs ++.. ys = x .:.. (xs ++.. ys)
_ ++.. _ = error "never occur"

(++.) :: (1 <= n, 1 <= m, 1 <= m + m') => TRange n m c x y -> TRange n' m' c y z -> TRange (n + n') (m + m') c x z
Nil ++. ys = loosenMax ys
x :.. xs ++. ys = x .:.. (xs ++. ys)
x :. xs ++. ys = x :. (xs ++. ys)

loosenMax :: 1 <= m' => TRange n m c x y -> TRange n m' c x y
loosenMax Nil = Nil
loosenMax (x :.. xs) = x :.. loosenMax xs
loosenMax (x :. xs) = x :. loosenMax xs

(.:..) :: 1 <= m => c x y -> TRange n m c y z -> TRange n (m + 1) c x z
x .:.. Nil = x :.. Nil
x .:.. ya@(_ :.. _) = x :.. ya
x .:.. (y :. ys) = x :. (y .:.. ys)
