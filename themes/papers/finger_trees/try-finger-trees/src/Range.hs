{-# LANGUAGE GADTs, DataKinds, TypeOperators, KindSignatures, StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Range where

import GHC.TypeLits

infixr 6 :., :..

data Range :: Nat -> Nat -> * -> * where
	Nil :: 0 <= m => Range 0 m a
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
	foldr (-<) z (x :.. xs) = x -< foldr (-<) z xs
	foldr _ _ _ = error "never occur"
	foldl (>-) z (x :.. xs) = foldl (>-) (z >- x) xs
	foldl _ _ _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	Foldable (Range (n - 1) (m - 1)) => Foldable (Range n m) where
	foldr (-<) z (x :. xs) = x -< foldr (-<) z xs
	foldr _ _ _ = error "never occur"
	foldl (>-) z (x :. xs) = foldl (>-) (z >- x) xs
	foldl _ _ _ = error "never occur"

class Loosen s t where
	loosen :: s a -> t a

instance Loosen (Range 0 0) (Range 0 0) where
	loosen = id

instance {-# OVERLAPPABLE #-} Loosen (Range 0 0) (Range 0 (m' - 1)) =>
	Loosen (Range 0 0) (Range 0 m') where
	loosen Nil = Nil
	loosen _ = error "never occur"

instance {-# OVERLAPPABLE #-} (1 <= m', Loosen (Range 0 (m - 1)) (Range 0 (m' - 1))) =>
	Loosen (Range 0 m) (Range 0 m') where
	loosen Nil = Nil
	loosen (x :.. xs) = x :.. loosen xs
	loosen _ = error "never occur"

instance {-# OVERLAPPABLE #-} (1 <= m', Loosen (Range (n - 1) (m - 1)) (Range 0 (m' - 1))) =>
	Loosen (Range n m) (Range 0 m') where
	loosen (x :. xs) = x :.. loosen xs
	loosen _ = error "never occur"

instance {-# OVERLAPPABLE #-} Loosen (Range (n - 1) (m - 1)) (Range (n' - 1) (m' - 1)) =>
	Loosen (Range n m) (Range n' m') where
	loosen (x :. xs) = x :. loosen xs
	loosen _ = error "never occur"
