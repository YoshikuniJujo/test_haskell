{-# LANGUAGE GADTs, DataKinds, TypeOperators, KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fplugin=TypeCheck.Nat #-}

module Range where

import GHC.TypeLits

infixr 6 :., :..

data Range :: Nat -> Nat -> * -> * where
	Nil :: Range 0 m a
	(:..) :: 1 <= m => a -> Range 0 (m - 1) a -> Range 0 m a
	(:.) :: a -> Range (n - 1) (m - 1) a -> Range n m a

deriving instance Show a => Show (Range n m a)

instance Foldable (Range 0 0) where
	foldr _ z Nil = z
	foldr _ _ _ = error "never occur"
	foldl _ z Nil = z
	foldl _ _ _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	Foldable (Range 0 (m - 1)) => Foldable (Range 0 m) where
	foldr _ z Nil = z
	foldr (-<) z (x :.. xs) = x -< foldr (-<) z xs
	foldr _ _ _ = error "never occur"
	foldl _ z Nil = z
	foldl (>-) z (x :.. xs) = foldl (>-) (z >- x) xs
	foldl _ _ _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	Foldable (Range (n - 1) (m - 1)) => Foldable (Range n m) where
	foldr (-<) z (x :. xs) = x -< foldr (-<) z xs
	foldr _ _ _ = error "never occur"
	foldl (>-) z (x :. xs) = foldl (>-) (z >- x) xs
	foldl _ _ _ = error "never occur"

class Loosen n m n' m' where
	loosen :: Range n m a -> Range n' m' a

instance Loosen 0 0 0 0 where
	loosen = id

instance {-# OVERLAPPABLE #-} (1 <= m', Loosen 0 0 0 (m' - 1)) =>
	Loosen 0 0 0 m' where
	loosen Nil = Nil
	loosen _ = error "never occur"

instance {-# OVERLAPPABLE #-} (1 <= m', Loosen 0 (m - 1) 0 (m' - 1)) =>
	Loosen 0 m 0 m' where
	loosen Nil = Nil
	loosen (x :.. xs) = x :.. loosen xs
	loosen _ = error "never occur"

instance {-# OVERLAPPABLE #-} (1 <= m', Loosen (n - 1) (m - 1) 0 (m' - 1)) =>
	Loosen n m 0 m' where
	loosen (x :. xs) = x :.. loosen xs
	loosen _ = error "never occur"

instance {-# OVERLAPPABLE #-} Loosen (n - 1) (m - 1) (n' - 1) (m' - 1) =>
	Loosen n m n' m' where
	loosen (x :. xs) = x :. loosen xs
	loosen _ = error "never occur"

infixr 5 ++.

(++.) :: 1 <= m => Range n m a -> Range n' m' a -> Range (n + n') (m + m') a
Nil ++. ys = loosenMax ys
-- Nil ++. ys = loosen ys
-- x :.. xs ++. ys = x .:.. (xs ++. ys)
-- x :. xs ++. ys = x :. (xs ++. ys)

loosenMax :: 1 <= m' => Range n m a -> Range n m' a
loosenMax Nil = Nil
loosenMax (x :.. xs) = x :.. loosenMax xs
loosenMax (x :. xs) = x :. loosenMax xs

infixr 5 .:..

(.:..) :: 1 <= m => a -> Range n (m - 1) a -> Range n m a
x .:.. Nil = x :.. Nil
x .:.. ya@(_ :.. _) = x :.. ya
x .:.. (y :. ys) = x :. (y .:.. ys)
