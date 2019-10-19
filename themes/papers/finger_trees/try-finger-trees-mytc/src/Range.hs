{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs, DataKinds, TypeOperators, KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fplugin=TypeCheck.Nat #-}

module Range where

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

class LoosenLMin n m n' where loosenLMin :: RangeL n m a -> RangeL n' m a

instance 1 <= m => LoosenLMin n m 0 where
	loosenLMin NilL = NilL
	loosenLMin (x :.. xs) = x :.. xs
	loosenLMin (x :. xs) = x :.. loosenLMin xs

instance {-# OVERLAPPABLE #-} (1 <= m, n' <= n, LoosenLMin (n - 1) (m - 1) (n' - 1)) =>
	LoosenLMin n m n' where
	loosenLMin (x :. xs) = x :. loosenLMin xs
	loosenLMin _ = error "never occur"

loosenLMax :: 1 <= m' => RangeL n m a -> RangeL n m' a
loosenLMax NilL = NilL
loosenLMax (x :.. xs) = x :.. loosenLMax xs
loosenLMax (x :. xs) = x :. loosenLMax xs

loosenL :: (1 <= m', LoosenLMin n m n') => RangeL n m a -> RangeL n' m' a
loosenL = loosenLMax . loosenLMin

infixr 5 ++.

(++.) :: 1 <= m => RangeL n m a -> RangeL n' m' a -> RangeL (n + n') (m + m') a
NilL ++. ys = loosenLMax ys
x :.. xs ++. ys = x .:.. (xs ++. ys)
x :. xs ++. ys = x :. (xs ++. ys)

infixr 5 .:..

(.:..) :: 1 <= m => a -> RangeL n (m - 1) a -> RangeL n m a
x .:.. NilL = x :.. NilL
x .:.. ya@(_ :.. _) = x :.. ya
x .:.. (y :. ys) = x :. (y .:.. ys)

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

class LoosenRMin n m n' where loosenRMin :: RangeR n m a -> RangeR n' m a

instance 1 <= m => LoosenRMin n m 0 where
	loosenRMin NilR = NilR
	loosenRMin (xs :++ x) = xs :++ x
	loosenRMin (xs :+ x) = loosenRMin xs :++ x

instance {-# OVERLAPPABLE #-} (1 <= m, n' <= n, LoosenRMin (n - 1) (m - 1) (n' - 1)) =>
	LoosenRMin n m n' where
	loosenRMin (xs :+ x) = loosenRMin xs :+ x
	loosenRMin _ = error "never occur"

loosenRMax :: 1 <= m' => RangeR n m a -> RangeR n m' a
loosenRMax NilR = NilR
loosenRMax (xs :++ x) = loosenRMax xs :++ x
loosenRMax (xs :+ x) = loosenRMax xs :+ x

loosenR :: (1 <= m', LoosenRMin n m n') => RangeR n m a -> RangeR n' m' a
loosenR = loosenRMax . loosenRMin

infixl 5 +++

(+++) :: 1 <= mm => RangeR nn mm a -> RangeR nn' mm' a -> RangeR (nn + nn') (mm + mm') a
ys +++ NilR = loosenRMax ys
ys +++ (xs :++ x) = (ys +++ xs) .:++ x
ys +++ (xs :+ x) = (ys +++ xs) :+ x

infixl 5 .:++

(.:++) :: 1 <= m => RangeR n (m - 1) a -> a -> RangeR n m a
NilR .:++ x = NilR :++ x
ya@(_ :++ _) .:++ x = ya :++ x
(ys :+ y) .:++ x = (ys .:++ y) :+ x

class RightToLeft n m n' m' where
	rightToLeftGen :: RangeL n m a -> RangeR n' m' a -> RangeL (n + n') (m + m') a

instance RightToLeft n m 0 0 where
	rightToLeftGen l NilR = l
	rightToLeftGen _ _ = error "never occur"

-- instance RightToLeft n m

{-
rightToLeftGen :: forall n m n' m' a . 1 <= m => RangeL n m a -> RangeR n' m' a -> RangeL (n + n') (m + m') a
rightToLeftGen l NilR = loosenLMax l
rightToLeftGen l (xs :+ x) = rightToLeftGen (x :. l :: RangeL (n + 1) (m + 1) a) xs
rightToLeftGen l (xs :++ x) = rightToLeftGen (x :.. l :: RangeL 0 (
-}
