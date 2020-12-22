{-# LANGUAGE ScopedTypeVariables, InstanceSigs #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances,
	UndecidableInstances #-}
{-# OPTIOnS_GHC -Wall -fno-warn-tabs -fplugin=Plugin.TypeCheck.Nat.Simple #-}

module Data.List.Range (
	-- * RANGED LIST LEFT
	module Data.List.Range.RangeL,
	-- * RANGED LIST RIGHT
	module Data.List.Range.RangeR,
	-- * LEFT TO RIGHT
	LeftToRight, (++.+), leftToRight,
	-- * RIGHT TO LEFT
	RightToLeft, (++..), rightToLeft ) where

import GHC.TypeNats

import Data.List.Range.RangeL
import Data.List.Range.RangeR

infixl 5 ++.+

class LeftToRight n m v w where
	(++.+) :: RangeR n m a -> RangeL v w a -> RangeR (n + v) (m + w) a

instance LeftToRight 0 m 0 0 where n ++.+ _ = n

instance {-# OVERLAPPABLE #-} LeftToRight n m 0 0 where n ++.+ _ = n

instance {-# OVERLAPPABLE #-}
	(LoosenRMax 0 m (m + w), LeftToRight 0 (m + 1) 0 (w - 1)) =>
	LeftToRight 0 m 0 w where
	(++.+) :: forall a . RangeR 0 m a -> RangeL 0 w a -> RangeR 0 (m + w) a
	n ++.+ NilL = loosenRMax n :: RangeR 0 (m + w) a
	n ++.+ x :.. v = (n :++ x :: RangeR 0 (m + 1) a) ++.+ v
	_ ++.+ _ = error "never occur"

instance {-# OVERLAPPABLE #-} (
	LoosenRMax n m (m + w), PushR (n - 1) (m - 1),
	LeftToRight n (m + 1) 0 (w - 1) ) =>
	LeftToRight n m 0 w where
	(++.+) :: forall a . RangeR n m a -> RangeL 0 w a -> RangeR n (m + w) a
	n ++.+ NilL = loosenRMax n :: RangeR n (m + w) a
	n ++.+ x :.. v= (n .:++ x :: RangeR n (m + 1) a) ++.+ v
	_ ++.+ _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	LeftToRight (n + 1) (m + 1) (v - 1) (w - 1) =>
	LeftToRight n m v w where
	(++.+) :: forall a .
		RangeR n m a -> RangeL v w a -> RangeR (n + v) (m + w) a
	n ++.+ x :. v = (n :+ x :: RangeR (n + 1) (m + 1) a) ++.+ v
	_ ++.+ _ = error "never occur"

leftToRight ::
	forall n m a . LeftToRight 0 0 n m => RangeL n m a -> RangeR n m a
leftToRight = ((NilR :: RangeR 0 0 a) ++.+)

infixr 5 ++..

class RightToLeft n m v w where
	(++..) :: RangeR n m a -> RangeL v w a -> RangeL (n + v) (m + w) a

instance RightToLeft 0 0 0 w where _ ++.. v = v

instance {-# OVERLAPPABLE #-} RightToLeft 0 0 v w where _ ++.. v = v

instance {-# OVERLAPPABLE #-}
	(LoosenLMax 0 w (m + w), RightToLeft 0 (m - 1) 0 (w + 1)) =>
	RightToLeft 0 m 0 w where
	(++..) :: forall a . RangeR 0 m a -> RangeL 0 w a -> RangeL 0 (m + w) a
	NilR ++.. v = loosenLMax v :: RangeL 0 (m + w) a
	n :++ x ++.. v = n ++.. (x :.. v :: RangeL 0 (w + 1) a)
	_ ++.. _ = error "never occur"

instance {-# OVERLAPPABLE #-} (
	LoosenLMax v w (m + w), PushL (v - 1) (w - 1),
	RightToLeft 0 (m - 1) v (w + 1) ) =>
	RightToLeft 0 m v w where
	(++..) :: forall a . RangeR 0 m a -> RangeL v w a -> RangeL v (m + w) a
	NilR ++.. v = loosenLMax v :: RangeL v (m + w) a
	n :++ x ++.. v = n ++.. (x .:.. v :: RangeL v (w + 1) a)
	_ ++.. _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	RightToLeft (n - 1) (m - 1) (v + 1) (w + 1) =>
	RightToLeft n m v w where
	(++..) :: forall a .
		RangeR n m a -> RangeL v w a -> RangeL (n + v) (m + w) a
	n :+ x ++.. v = n ++.. (x :. v :: RangeL (v + 1) (w + 1) a)
	_ ++.. _ = error "never occur"

rightToLeft ::
	forall n m a . RightToLeft n m 0 0 => RangeR n m a -> RangeL n m a
rightToLeft = (++.. (NilL :: RangeL 0 0 a))
