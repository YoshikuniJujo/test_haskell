{-# LANGUAGE ScopedTypeVariables, InstanceSigs #-}
{-# LANGUAGE GADTs, DataKinds, TypeOperators, KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fplugin=TypeCheck.Nat #-}

module TypeCheck.Test where

import GHC.TypeLits

infixr 6 :., :..

data RangeL :: Nat -> Nat -> * -> * where
	NilL :: RangeL 0 m a
	(:..) :: 1 <= m => a -> RangeL 0 (m - 1) a -> RangeL 0 m a
	(:.) :: a -> RangeL (n - 1) (m - 1) a -> RangeL n m a

infixr 5 .:..

class PushL m where
	(.:..) :: a -> RangeL n m a -> RangeL n (m + 1) a

instance PushL 0 where
	x .:.. NilL = x :.. NilL
	_ .:.. _ = error "never occur"

instance {-# OVERLAPPABLE #-} (1 <= m, 1 <= m + 1) => PushL m where
	x .:.. NilL = x :.. NilL
	x .:.. ya@(_ :.. _) = x :.. ya
	x .:.. (y :. ys) = x :. (y .:.. ys)

{-
(.:..) :: (1 <= m, 1 <= m - 1, 1 <= (m - 1) - 1) => a -> RangeL n (m - 1) a -> RangeL n m a
x .:.. NilL = x :.. NilL
x .:.. ya@(_ :.. _) = x :.. ya
x .:.. (y :. ys) = x :. (y .:.. ys)
-}

{-
type Node = RangeL 2 3

class Nodes m m' where
	nodes :: RangeL 2 m a -> RangeL 1 m' (Node a)

instance Nodes 3 1 where
	nodes = (:. NilL)

instance {-# OVERLAPPABLE #-} (1 <= m', 1 <= (m' - 1), Nodes (m - 3) (m' - 1)) => Nodes m m' where
	nodes :: forall a . RangeL 2 m a -> RangeL 1 m' (Node a)
	nodes (a :. b :. NilL) = (a :. b :. NilL) :. NilL
	nodes (a :. b :. c :.. NilL) = (a :. b :. c :.. NilL) :. NilL
	nodes (a :. b :. c :.. d :.. NilL) = (a :. b :. NilL) :. (c :. d :. NilL) :.. NilL
	nodes (a :. b :. c :.. d :.. e :.. xs) = (a :. b :. c :.. NilL) .:.. (nodes (d :. e :. xs :: RangeL 2 (m - 3) a) :: RangeL 1 (m' - 1) (Node a))
	-}
