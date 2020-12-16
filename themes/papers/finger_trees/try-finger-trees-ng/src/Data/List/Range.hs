{-# LANGUAGE ScopedTypeVariables, InstanceSigs #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances,
	UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fplugin=Plugin.TypeCheck.Nat.Simple #-}

module Data.List.Range (
	RangeL(..), PushL, AddL, LoosenLMin, LoosenLMax,
	(.:..), (++.), loosenLMax, loosenL,
	RangeR(..), LoosenRMin, LoosenRMax, loosenRMax, loosenR,
	LeftToRight, RightToLeft, leftToRight, rightToLeft, (++.+), (++..)
	) where

import GHC.TypeLits (type (+), type (-))

import Data.List.Range.RangeL (
	RangeL(..), PushL, AddL, LoosenLMin, LoosenLMax,
	(.:..), (++.), loosenLMax, loosenL )
import Data.List.Range.RangeR (
	RangeR(..), PushR, LoosenRMin, LoosenRMax, (.:++), loosenRMax, loosenR )

---------------------------------------------------------------------------

-- * LEFT TO RIGHT
--	+ CLASS
--	+ INSTANCE
--	+ FUNCTION
-- * RIGHT TO LEFT
--	+ CLASS
--	+ INSTANCE
--	+ FUNCTION

---------------------------------------------------------------------------
-- LEFT TO RIGHT
---------------------------------------------------------------------------

-- CLASS

infixl 5 ++.+

class LeftToRight n m n' m' where
	(++.+) :: RangeR n m a -> RangeL n' m' a -> RangeR (n + n') (m + m') a

-- INSTANCE

instance LeftToRight 0 m 0 0 where r ++.+ _ =  r

instance {-# OVERLAPPABLE #-} LeftToRight n m 0 0 where r ++.+ _ = r

instance {-# OVERLAPPABLE #-}
	(LoosenRMax 0 m (m + m'), LeftToRight 0 (m + 1) 0 (m' - 1)) =>
	LeftToRight 0 m 0 m' where
	(++.+) ::
		forall a . RangeR 0 m a -> RangeL 0 m' a -> RangeR 0 (m + m') a
	r ++.+ NilL = loosenRMax r :: RangeR 0 (m + m') a
	r ++.+ (x :.. xs) = (r :++ x :: RangeR 0 (m + 1) a) ++.+ xs
	_ ++.+ _ = error "never occur"

instance {-# OVERLAPPABLE #-} (
	LoosenRMax n m (m + m'), PushR (n - 1) (m - 1),
	LeftToRight n (m + 1) 0 (m' - 1) ) =>
	LeftToRight n m 0 m' where
	(++.+) ::
		forall a . RangeR n m a -> RangeL 0 m' a -> RangeR n (m + m') a
	r ++.+ NilL = loosenRMax r :: RangeR n (m + m') a
	r ++.+ (x :.. xs) = (r .:++ x :: RangeR n (m + 1) a) ++.+ xs
	_ ++.+ _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	LeftToRight (n + 1) (m + 1) (n' - 1) (m' - 1) =>
	LeftToRight n m n' m' where
	(++.+) :: forall a .
		RangeR n m a -> RangeL n' m' a -> RangeR (n + n') (m + m') a
	r ++.+ (x :. xs) =
		(r :+ x :: RangeR (n + 1) (m + 1) a) ++.+ xs
	_ ++.+ _ = error "never occur"

-- FUNCTION

leftToRight ::
	forall n m a . LeftToRight 0 0 n m => RangeL n m a -> RangeR n m a
leftToRight = ((NilR :: RangeR 0 0 a) ++.+)

---------------------------------------------------------------------------
-- RIGHT TO LEFT
---------------------------------------------------------------------------

-- CLASS

infixr 5 ++..

class RightToLeft n m n' m' where
	(++..) :: RangeR n m a -> RangeL n' m' a -> RangeL (n + n') (m + m') a

-- INSTANCE

instance RightToLeft 0 0 0 m' where _ ++.. l =  l

instance {-# OVERLAPPABLE #-} RightToLeft 0 0 n' m' where _ ++.. l = l

instance {-# OVERLAPPABLE #-}
	(LoosenLMax 0 m' (m + m'), RightToLeft 0 (m - 1) 0 (m' + 1)) =>
	RightToLeft 0 m 0 m' where
	(++..) ::
		forall a . RangeR 0 m a -> RangeL 0 m' a -> RangeL 0 (m + m') a
	NilR ++.. l = loosenLMax l :: RangeL 0 (m + m') a
	(xs :++ x) ++.. l = xs ++.. (x :.. l :: RangeL 0 (m' + 1) a)
	_ ++.. _ = error "never occur"


instance {-# OVERLAPPABLE #-} (
	LoosenLMax n' m' (m + m'), PushL (n' - 1) (m' - 1),
	RightToLeft 0 (m - 1) n' (m' + 1)) =>
	RightToLeft 0 m n' m' where
	(++..) ::
		forall a . RangeR 0 m a -> RangeL n' m' a -> RangeL n' (m + m') a
	NilR ++.. l = loosenLMax l :: RangeL n' (m + m') a
	(xs :++ x) ++.. l = xs ++.. (x .:.. l :: RangeL n' (m' + 1) a)
	_ ++.. _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	RightToLeft (n - 1) (m - 1) (n' + 1) (m' + 1) =>
	RightToLeft n m n' m' where
	(++..) :: forall a .
		RangeR n m a -> RangeL n' m' a -> RangeL (n + n') (m + m') a
	(xs :+ x) ++.. l = xs ++.. (x :. l :: RangeL (n' + 1) (m' + 1) a)
	_ ++.. _ = error "never occur"

-- FUNCTION

rightToLeft ::
	forall n m a . RightToLeft n m 0 0 => RangeR n m a -> RangeL n m a
rightToLeft = (++.. (NilL :: RangeL 0 0 a))
