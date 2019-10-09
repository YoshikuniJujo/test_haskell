{-# LANGUAGE GADTs, DataKinds, TypeOperators, KindSignatures, StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fplugin=GHC.TypeLits.Normalise #-}

module Range where

import Control.Arrow

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

class Loosen s t where
	loosen :: s a -> t a

instance Loosen (Range 0 0) (Range 0 0) where
	loosen = id

instance {-# OVERLAPPABLE #-} (1 <= m', Loosen (Range 0 0) (Range 0 (m' - 1))) =>
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

uncons :: 1 <= n => Range n m a -> (a, Range (n - 1) (m - 1) a)
uncons (x :. xs) = (x, xs)

cons :: a -> Range (n - 1) (m - 1) a -> Range n m a
cons x xs = x :. xs

class Init n m where
	initR :: Range n m a -> Range (n - 1) (m - 1) a

instance Init 1 1 where
	initR (_ :. Nil) = Nil
	initR _ = error "never occur"

instance {-# OVERLAPPABLE #-} Init 1 (m - 1) => Init 1 m where
	initR (_ :. Nil) = Nil
	initR (x :. y :.. xs) = x :.. initR (y :. xs)
	initR _ = error "never occur"

instance {-# OVERLAPPABLE #-} Init (n - 1) (m - 1) => Init n m where
	initR (x :. xs) = x :. initR xs
	initR _ = error "never occur"

class Unsnoc n m where
	unsnoc :: Range n m a -> (Range (n - 1) (m - 1) a, a)

instance Unsnoc 1 1 where
	unsnoc (x :. Nil) = (Nil, x)
	unsnoc _ = error "never occur"

instance {-# OVERLAPPABLE #-} Unsnoc 1 (m - 1) => Unsnoc 1 m where
	unsnoc (x :. Nil) = (Nil, x)
	unsnoc (x :. y :.. xs) = (x :..) `first` unsnoc (y :. xs)
	unsnoc _ = error "never occur"

instance {-# OVERLAPPABLE #-} Unsnoc (n - 1) (m - 1) => Unsnoc n m where
	unsnoc (x :. xs) = (x :.) `first` unsnoc xs
	unsnoc _ = error "never occur"

class FromList n m where
	fromList :: [a] -> Range n m a

instance FromList 0 0 where
	fromList [] = Nil
	fromList _ = error "bad"

instance {-# OVERLAPPABLE #-} (1 <= m, FromList 0 (m - 1)) => FromList 0 m where
	fromList [] = Nil
	fromList (x : xs) = x :.. fromList xs

instance {-# OVERLAPPABLE #-} FromList (n - 1) (m - 1) => FromList n m where
	fromList [] = error "bad"
	fromList (x : xs) = x :. fromList xs

infixr 5 ++.., ++.

(++..) :: 1 <= m + m' => Range 0 m a -> Range n' m' a -> Range n' (m + m') a
Nil ++.. ys = loosenMax ys
x :.. xs ++.. ys = x .:.. (xs ++.. ys)
_ ++.. _ = error "never occur"

(++.) :: (1 <= n, 1 <= m, 1 <= m + m') => Range n m a -> Range n' m' a -> Range (n + n') (m + m') a
x :. xs ++. ys = x :. (xs ++. ys)

infixr 5 .:..

loosenMax :: 1 <= m' => Range n m a -> Range n m' a
loosenMax Nil = Nil
loosenMax (x :.. xs) = x :.. loosenMax xs
loosenMax (x :. xs) = x :. loosenMax xs

(.:..) :: 1 <= m => a -> Range n m a -> Range n (m + 1) a
x .:.. Nil = x :.. Nil
x .:.. ya@(_ :.. _) = x :.. ya
x .:.. (y :. ys) = x :. (y .:.. ys)
