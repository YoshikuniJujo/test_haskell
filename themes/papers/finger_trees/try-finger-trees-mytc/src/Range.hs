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

class LoosenMin n m n' where loosenMin :: Range n m a -> Range n' m a

instance 1 <= m => LoosenMin n m 0 where
	loosenMin Nil = Nil
	loosenMin (x :. xs) = x :.. loosenMin xs
	loosenMin (x :.. xs) = x :.. xs

instance {-# OVERLAPPABLE #-} (1 <= m, n' <= n, LoosenMin (n - 1) (m - 1) (n' - 1)) =>
	LoosenMin n m n' where
	loosenMin (x :. xs) = x :. loosenMin xs
	loosenMin _ = error "never occur"

loosenMax :: 1 <= m' => Range n m a -> Range n m' a
loosenMax Nil = Nil
loosenMax (x :.. xs) = x :.. loosenMax xs
loosenMax (x :. xs) = x :. loosenMax xs

loosen :: (1 <= m', LoosenMin n m n') => Range n m a -> Range n' m' a
loosen = loosenMax . loosenMin

infixr 5 ++.

(++.) :: 1 <= m => Range n m a -> Range n' m' a -> Range (n + n') (m + m') a
Nil ++. ys = loosenMax ys
-- Nil ++. ys = loosen ys
-- x :.. xs ++. ys = x .:.. (xs ++. ys)
-- x :. xs ++. ys = x :. (xs ++. ys)

infixr 5 .:..

(.:..) :: 1 <= m => a -> Range n (m - 1) a -> Range n m a
x .:.. Nil = x :.. Nil
x .:.. ya@(_ :.. _) = x :.. ya
x .:.. (y :. ys) = x :. (y .:.. ys)
