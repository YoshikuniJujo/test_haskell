{-# LANGUAGE ScopedTypeVariables, InstanceSigs #-}
{-# LANGUAGE GADTs, KindSignatures, DataKinds, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
-- {-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fplugin=TypeCheck.Nat #-}

module TypeCheck.Test.TRange where

import GHC.TypeLits

import TypeCheck.Test.TFoldable

--------------------------------------------------------------------------------
-- TRangeL
--------------------------------------------------------------------------------

infixr 6 :.., :.

data TRangeL :: Nat -> Nat -> (* -> * -> *) -> * -> * -> * where
	NilL :: TRangeL 0 m c a a
	(:..) :: 1 <= m => c a x -> TRangeL 0 (m - 1) c x b -> TRangeL 0 m c a b
	(:.) :: c a x -> TRangeL (n - 1) (m - 1) c x b -> TRangeL n m c a b

instance TFoldable (TRangeL 0 0) where
	tfoldr _ z NilL = z
	tfoldr _ _ _ = error "never occur"
	tfoldl _ z NilL = z
	tfoldl _ _ _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	TFoldable (TRangeL 0 (m - 1)) => TFoldable (TRangeL 0 m) where
	tfoldr _ z NilL = z
	tfoldr (-<) z (x :.. xs) = x -< tfoldr (-<) z xs
	tfoldr _ _ _ = error "never occur"
	tfoldl _ z NilL = z
	tfoldl (>-) z (x :.. xs) = tfoldl (>-) (z >- x) xs
	tfoldl _ _ _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	TFoldable (TRangeL (n - 1) (m - 1)) => TFoldable (TRangeL n m) where
	tfoldr (-<) z (x :. xs) = x -< tfoldr (-<) z xs
	tfoldr _ _ _ = error "never occur"
	tfoldl (>-) z (x :. xs) = tfoldl (>-) (z >- x) xs
	tfoldl _ _ _ = error "never occur"

infixr 5 .:..

class PushL n m where
	(.:..) :: c x y -> TRangeL n m c y z -> TRangeL n (m + 1) c x z

instance 1 <= m + 1 => PushL 0 m where
	(.:..) = (:..)

instance {-# OVERLAPPABLE #-} PushL (n - 1) (m - 1) => PushL n m where
	x .:.. (y :. ys) = x :. (y .:.. ys)
	_ .:.. _ = error "never occur"

class LoosenLMin n m n' where
	loosenLMin :: TRangeL n m c x y -> TRangeL n' m c x y

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

class LoosenLMax n m m' where
	loosenLMax :: TRangeL n m c x y -> TRangeL n m' c x y

instance LoosenLMax 0 0 m' where
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

loosenL :: (LoosenLMin n m n', LoosenLMax n' m m') =>
	TRangeL n m c x y -> TRangeL n' m' c x y
loosenL = loosenLMax . loosenLMin

newtype C a x y = C { getC :: a } deriving Show

cmap :: (a -> b) -> C a x y -> C b x y
cmap f (C x) = C $ f x

czipWith :: (a -> b -> c) -> C a x y -> C b y z -> C c x z
czipWith op (C x) (C y) = C $ x `op` y

sampleX :: C Int Bool Char
sampleX = C 11

sampleY :: C Int Char Double
sampleY = C 15

sampleZ :: C Int Double ()
sampleZ = C 3

sampleW :: C Int () Char
sampleW = C 111

sampleTRangeL :: TRangeL 2 5 (C Int) Bool ()
sampleTRangeL = sampleX :. sampleY :. sampleZ :.. NilL

sampleTRangeL2 :: TRangeL 1 3 (C Int) () Char
sampleTRangeL2 = sampleW :. NilL

infixr 5 ++.

class AddL n m n' m' where
	(++.) :: TRangeL n m c x y ->
		TRangeL n' m' c y z -> TRangeL (n + n') (m + m') c x z

instance AddL 0 0 n' m' where
	NilL ++. ys = ys
	_ ++. _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	(1 <= m + m', LoosenLMax n' m' (m + m'),
			PushL n' (m + m' - 1), AddL 0 (m - 1) n' m') =>
		AddL 0 m n' m' where
	(++.) :: forall c x y z . TRangeL 0 m c x y -> TRangeL n' m' c y z -> TRangeL n' (m + m') c x z
	NilL ++. ys = loosenLMax ys
	(x :.. xs) ++. ys = x ..:.. (xs ++. ys)
		where
		(..:..) :: forall w . c x w -> TRangeL n' (m + m' - 1) c w z -> TRangeL n' (m + m') c x z
		(..:..) = (.:..)
	_ ++. _ = error "never occur"

instance {-# OVERLAPPABLE #-} AddL (n - 1) (m - 1) n' m' => AddL n m n' m' where
	(x :. xs) ++. ys = x :. (xs ++. ys)
	_ ++. _ = error "never occur"

--------------------------------------------------------------------------------
-- TRangeR
--------------------------------------------------------------------------------

infixl 6 :+, :++

data TRangeR :: Nat -> Nat -> (* -> * -> *) -> * -> * -> * where
	NilR :: TRangeR 0 m c x x
	(:++) :: 1 <= m => TRangeR 0 (m - 1) c x y -> c y z -> TRangeR 0 m c x z
	(:+) :: TRangeR (n - 1) (m - 1) c x y -> c y z -> TRangeR n m c x z

-- deriving instance Show (c x y) => Show (TRangeR n m c x y)

instance TFoldable (TRangeR 0 0) where
	tfoldr _ z NilR = z
	tfoldr _ _ _ = error "never occur"
	tfoldl _ z NilR = z
	tfoldl _ _ _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	TFoldable (TRangeR 0 (m - 1)) => TFoldable (TRangeR 0 m) where
	tfoldr _ z NilR = z
	tfoldr (-<) z (xs :++ x) = tfoldr (-<) (x -< z) xs
	tfoldr _ _ _ = error "never occur"
	tfoldl _ z NilR = z
	tfoldl (>-) z (xs :++ x) = tfoldl (>-) z xs >- x
	tfoldl _ _ _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	TFoldable (TRangeR (n - 1) (m - 1)) => TFoldable (TRangeR n m) where
	tfoldr (-<) z (xs :+ x) = tfoldr (-<) (x -< z) xs
	tfoldr _ _ _ = error "never occur"
	tfoldl (>-) z (xs :+ x) = tfoldl (>-) z xs >- x
	tfoldl _ _ _ = error "never occur"

infixl 5 .:++

class TPushR n m where
	(.:++) :: TRangeR n m c x y -> c y z -> TRangeR n (m + 1) c x z

instance 1 <= m + 1 => TPushR 0 m where (.:++) = (:++)

instance {-# OVERLAPPABLE #-}
	(1 <= m + 1, TPushR (n - 1) (m - 1)) => TPushR n m where
	(xs :+ x) .:++ y = (xs .:++ x) :+ y
	_ .:++ _ = error "never occur"

class TLoosenRMin n m n' where
	tloosenRMin :: TRangeR n m c x y -> TRangeR n' m c x y

-- instance TLoosenRMin 0 0 0 where
