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
	leftToRight, rightToLeft ) where

import GHC.TypeLits (type (+), type (-), type (<=))

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

class LeftToRight n m n' m' where
	leftToRightGen :: RangeR n m a -> RangeL n' m' a -> RangeR (n + n') (m + m') a

-- INSTANCE

instance LeftToRight 0 m 0 0 where leftToRightGen r _ =  r

instance {-# OVERLAPPABLE #-} LeftToRight n m 0 0 where leftToRightGen r _ = r

instance {-# OVERLAPPABLE #-}
	(1 <= m + 1, 1 <= m + m', LoosenRMax 0 m (m + m'), LeftToRight 0 (m + 1) 0 (m' - 1)) =>
	LeftToRight 0 m 0 m' where
	leftToRightGen :: forall a . RangeR 0 m a -> RangeL 0 m' a -> RangeR 0 (m + m') a
	leftToRightGen r NilL = loosenRMax r :: RangeR 0 (m + m') a
	leftToRightGen r (x :.. xs) = leftToRightGen (r :++ x :: RangeR 0 (m + 1) a) xs
	leftToRightGen _ _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	(1 <= m + 1, 1 <= m + moops', LoosenRMax n m (m + moops'), PushR (n - 1) (m - 1), LeftToRight n (m + 1) 0 (moops' - 1)) =>
	LeftToRight n m 0 moops' where
	leftToRightGen :: forall a . RangeR n m a -> RangeL 0 moops' a -> RangeR n (m + moops') a
	leftToRightGen r NilL = loosenRMax r :: RangeR n (m + moops') a
	leftToRightGen r (x :.. xs) = leftToRightGen (r .:++ x :: RangeR n (m + 1) a) xs
	leftToRightGen _ _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	LeftToRight (n + 1) (m + 1) (n' - 1) (m' - 1) => LeftToRight n m n' m' where
	leftToRightGen :: forall a . RangeR n m a -> RangeL n' m' a -> RangeR (n + n') (m + m') a
	leftToRightGen r (x :. xs) = leftToRightGen (r :+ x :: RangeR (n + 1) (m + 1) a) xs
	leftToRightGen _ _ = error "never occur"

-- FUNCTION

leftToRight :: forall n m a . LeftToRight 0 0 n m => RangeL n m a -> RangeR n m a
leftToRight = leftToRightGen (NilR :: RangeR 0 0 a)

---------------------------------------------------------------------------
-- RIGHT TO LEFT
---------------------------------------------------------------------------

-- CLASS

class RightToLeft n m n' m' where
	rightToLeftGen :: RangeL n m a -> RangeR n' m' a -> RangeL (n + n') (m + m') a

-- INSTANCE

instance RightToLeft 0 m 0 0 where rightToLeftGen l _ =  l

instance {-# OVERLAPPABLE #-} RightToLeft n m 0 0 where rightToLeftGen l _ = l

instance {-# OVERLAPPABLE #-}
	(1 <= m + 1, 1 <= m + m', LoosenLMax 0 m (m + m'), RightToLeft 0 (m + 1) 0 (m' - 1)) =>
	RightToLeft 0 m 0 m' where
	rightToLeftGen :: forall a . RangeL 0 m a -> RangeR 0 m' a -> RangeL 0 (m + m') a
	rightToLeftGen l NilR = loosenLMax l :: RangeL 0 (m + m') a
	rightToLeftGen l (xs :++ x) = rightToLeftGen (x :.. l :: RangeL 0 (m + 1) a) xs
	rightToLeftGen _ _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	(1 <= m + 1, LoosenLMax n m (m + m'), PushL (n - 1) (m - 1), RightToLeft n (m + 1) 0 (m' - 1)) =>
	RightToLeft n m 0 m' where
	rightToLeftGen :: forall a . RangeL n m a -> RangeR 0 m' a -> RangeL n (m + m') a
	rightToLeftGen l NilR = loosenLMax l :: RangeL n (m + m') a
	rightToLeftGen l (xs :++ x) = rightToLeftGen (x .:.. l :: RangeL n (m + 1) a) xs
	rightToLeftGen _ _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	RightToLeft (n + 1) (m + 1) (n' - 1) (m' - 1) => RightToLeft n m n' m' where
	rightToLeftGen :: forall a . RangeL n m a -> RangeR n' m' a -> RangeL (n + n') (m + m') a
	rightToLeftGen l (xs :+ x) = rightToLeftGen (x :. l :: RangeL (n + 1) (m + 1) a) xs
	rightToLeftGen _ _ = error "never occur"

-- FUNCTION

rightToLeft :: forall n m a . RightToLeft 0 0 n m => RangeR n m a -> RangeL n m a
rightToLeft = rightToLeftGen (NilL :: RangeL 0 0 a)
