{-# LANGUAGE ScopedTypeVariables, InstanceSigs #-}
{-# LANGUAGE GADTs, DataKinds, TypeOperators, KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fplugin=TypeCheck.Nat #-}

module TypeCheck.Test.Range (
	RangeL(..), PushL(..), loosenL, AddL(..), LoosenLMax(..), LoosenLMin(..),
	RangeR(..), PushR(..), loosenR, LoosenRMax(..), LoosenRMin(..),
	leftToRight, rightToLeft ) where

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

class AddL n m n' m' where
	(++.) :: RangeL n m a -> RangeL n' m' a -> RangeL (n + n') (m + m') a

instance AddL 0 0 n' m' where
	NilL ++. ys = ys
	_ ++. _ = error "never occur"

instance  {-# OVERLAPPABLE #-} (1 <= m + m', LoosenLMax n' m' (m + m'), PushL n' (m + m' - 1), AddL 0 (m - 1) n' m') => AddL 0 m n' m' where
	(++.) :: forall a . RangeL 0 m a -> RangeL n' m' a -> RangeL n' (m + m') a
	NilL ++. ys = loosenLMax ys
	(x :.. xs) ++. ys = x .:.. (xs ++. ys :: RangeL n' (m + m' - 1) a)
	_ ++. _ = error "never occur"

instance {-# OVERLAPPABLE #-} (AddL (n - 1) (m - 1) n' m') => AddL n m n' m' where
	(x :. xs) ++. ys = x :. (xs ++. ys)
	_ ++. _ = error "never occur"
	
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

class LoosenRMin n m n' where loosenRMin :: RangeR n m a -> RangeR n' m a

instance LoosenRMin 0 m 0 where
	loosenRMin NilR = NilR
	loosenRMin xa@(_ :++ _) = xa
	loosenRMin _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	(1 <= m, LoosenRMin (n - 1) (m - 1) 0) => LoosenRMin n m 0 where
	loosenRMin (xs :+ x) = loosenRMin xs :++ x
	loosenRMin _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	LoosenRMin (n - 1) (m - 1) (n' - 1) => LoosenRMin n m n' where
	loosenRMin (xs :+ x) = loosenRMin xs :+ x
	loosenRMin _ = error "never occur"

class LoosenRMax n m m' where loosenRMax :: RangeR n m a -> RangeR n m' a

instance LoosenRMax 0 0 m where
	loosenRMax NilR = NilR
	loosenRMax _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	(1 <= m', LoosenRMax 0 (m - 1) (m' - 1)) => LoosenRMax 0 m m' where
	loosenRMax NilR = NilR
	loosenRMax (xs :++ x) = loosenRMax xs :++ x
	loosenRMax _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	LoosenRMax (n - 1) (m - 1) (m' - 1) => LoosenRMax n m m' where
	loosenRMax (xs :+ x) = loosenRMax xs :+ x
	loosenRMax _ = error "never occur"

loosenR :: (LoosenRMin n m n', LoosenRMax n' m m') => RangeR n m a -> RangeR n' m' a
loosenR = loosenRMax . loosenRMin

class LeftToRight n m n' m' where
	leftToRightGen :: RangeR n m a -> RangeL n' m' a -> RangeR (n + n') (m + m') a

instance LeftToRight 0 m 0 0 where
	leftToRightGen r _ = r

instance {-# OVERLAPPABLE #-} LeftToRight n m 0 0 where
	leftToRightGen r _ = r

instance {-# OVERLAPPABLE #-}
	(1 <= m + 1, 1 <= m + m', LoosenRMax 0 m (m + m'), LeftToRight 0 (m + 1) 0 (m' - 1)) => LeftToRight 0 m 0 m' where
	leftToRightGen :: forall a . RangeR 0 m a -> RangeL 0 m' a -> RangeR 0 (m + m') a
	leftToRightGen r NilL = loosenRMax r :: RangeR 0 (m + m') a
	leftToRightGen r (x :.. xs) = leftToRightGen (r .:++ x :: RangeR 0 (m + 1) a) xs
	leftToRightGen _ _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	(1 <= m + 1, 1 <= m + m', LoosenRMax n m (m + m'), PushR (n - 1) (m - 1),  LeftToRight n (m + 1) 0 (m' - 1)) => LeftToRight n m 0 m' where
	leftToRightGen :: forall a . RangeR n m a -> RangeL 0 m' a -> RangeR n (m + m') a
	leftToRightGen r NilL = loosenRMax r :: RangeR n (m + m') a
	leftToRightGen r (x :.. xs) = leftToRightGen (r .:++ x :: RangeR n (m + 1) a) xs
	leftToRightGen _ _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	LeftToRight (n + 1) (m + 1) (n' - 1)  (m' - 1) => LeftToRight n m n' m' where
	leftToRightGen :: forall a . RangeR n m a -> RangeL n' m' a -> RangeR (n + n') (m + m') a
	leftToRightGen r (x :. xs) = leftToRightGen (r :+ x :: RangeR (n + 1) (m + 1) a) xs
	leftToRightGen _ _ = error "never occur"

leftToRight :: forall n m a . LeftToRight 0 0 n m => RangeL n m a -> RangeR n m a
leftToRight = leftToRightGen (NilR :: RangeR 0 0 a)

class RightToLeft n m n' m' where
	rightToLeftGen :: RangeL n m a -> RangeR n' m' a -> RangeL (n + n') (m + m') a

instance RightToLeft 0 m 0 0 where
	rightToLeftGen l _ = l

instance {-# OVERLAPPABLE #-} RightToLeft n m 0 0 where
	rightToLeftGen l _ = l

instance {-# OVERLAPPABLE #-}
	(1 <= m + 1, 1 <= m + m', LoosenLMax 0 m (m + m'), RightToLeft 0 (m + 1) 0 (m' - 1)) => RightToLeft 0 m 0 m' where
	rightToLeftGen :: forall a . RangeL 0 m a -> RangeR 0 m' a -> RangeL 0 (m + m') a
	rightToLeftGen l NilR = loosenLMax l :: RangeL 0 (m + m') a
	rightToLeftGen l (xs :++ x) = rightToLeftGen (x .:.. l :: RangeL 0 (m + 1) a) xs
	rightToLeftGen _ _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	(1 <= m + 1, LoosenLMax n m (m + m'), PushL (n - 1) (m - 1), RightToLeft n (m + 1) 0 (m' - 1)) => RightToLeft n m 0 m' where
	rightToLeftGen :: forall a . RangeL n m a -> RangeR 0 m' a -> RangeL n (m + m') a
	rightToLeftGen l NilR = loosenLMax l :: RangeL n (m + m') a
	rightToLeftGen l (xs :++ x) = rightToLeftGen (x .:.. l :: RangeL n (m + 1) a) xs
	rightToLeftGen _ _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	RightToLeft (n + 1) (m + 1) (n' - 1) (m' - 1) => RightToLeft n m n' m' where
	rightToLeftGen :: forall a . RangeL n m a -> RangeR n' m' a -> RangeL (n + n') (m + m') a
	rightToLeftGen l (xs :+ x) = rightToLeftGen (x :. l :: RangeL (n + 1) (m + 1) a) xs
	rightToLeftGen _ _ = error "never occur"

rightToLeft :: forall n m a . RightToLeft 0 0 n m => RangeR n m a -> RangeL n m a
rightToLeft = rightToLeftGen (NilL :: RangeL 0 0 a)
