{-# LANGUAGE ScopedTypeVariables, InstanceSigs #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances,
	UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fplugin=New.TypeCheck.Nat #-}

module Data.List.Range (
	module Data.List.RangeL,
	module Data.List.RangeR
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
