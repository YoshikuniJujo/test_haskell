{-# LANGUAGE DataKinds, KindSignatures, TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances,
	UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.List.Range.RangeR (
	RangeR(..), LoosenRMax, loosenRMax, loosenR ) where

import GHC.TypeLits (Nat, type (-), type (<=))

---------------------------------------------------------------------------

-- * RANGE R
-- * FOLDABLE
-- * LOOSEN R
--	+ LOOSEN R MIN
--	+ LOOSEN R MAX
--	+ FUNCTION LOOSEN R

---------------------------------------------------------------------------
-- RANGE R
---------------------------------------------------------------------------

infixl 6 :+, :++

data RangeR :: Nat -> Nat -> * -> * where
	NilR :: RangeR 0 m a
	(:++) :: 1 <= m => RangeR 0 (m - 1) a -> a -> RangeR 0 m a
	(:+) :: RangeR (n - 1) (m - 1) a -> a -> RangeR n m a

deriving instance Show a => Show (RangeR n m a)

---------------------------------------------------------------------------
-- FOLDABLE
---------------------------------------------------------------------------

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

---------------------------------------------------------------------------
-- LOOSEN R
---------------------------------------------------------------------------

-- LOOSEN R MIN

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

-- LOOSEN R MAX

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

-- FUNCTION LOOSEN R

loosenR :: (LoosenRMin n m n', LoosenRMax n' m m') => RangeR n m a -> RangeR n' m' a
loosenR = loosenRMax . loosenRMin
