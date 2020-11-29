{-# LANGUAGE ScopedTypeVariables, InstanceSigs #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances,
	UndecidableInstances #-}
-- {-# OPTIONS_GHC -Wall -fno-warn-tabs -fplugin=New.TypeCheck.Nat #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fplugin=TypeCheck.Nat #-}

module Data.List.Range (
	module Data.List.RangeL,
	module Data.List.RangeR,
	leftToRight
	) where

import GHC.TypeLits

import Data.List.RangeL
import Data.List.RangeR

class LeftToRight n m n' m' where
	leftToRightGen :: RangeR n m a -> RangeL n' m' a -> RangeR (n + n') (m + m') a

instance LeftToRight 0 m 0 0 where
	leftToRightGen r _ =  r

instance {-# OVERLAPPABLE #-} LeftToRight n m 0 0 where
	leftToRightGen r _ = r

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

leftToRight :: forall n m a . LeftToRight 0 0 n m => RangeL n m a -> RangeR n m a
leftToRight = leftToRightGen (NilR :: RangeR 0 0 a)

infixl 5 .:++

class PushR n m where (.:++) :: RangeR n m a -> a -> RangeR n (m + 1) a

instance 1 <= m + 1 => PushR 0 m where (.:++) = (:++)

instance {-# OVERLAPPABLE #-} (1 <= m + 1, PushR (n - 1) (m - 1)) => PushR n m where
	(xs :+ x) .:++ y = (xs .:++ x) :+ y
	_ .:++ _ = error "never occur"
