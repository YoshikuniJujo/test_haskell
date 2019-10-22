{-# LANGUAGE ScopedTypeVariables, InstanceSigs #-}
{-# LANGUAGE GADTs, DataKinds, TypeOperators, KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fplugin=TypeCheck.Nat #-}

module TypeCheck.Test where

import GHC.TypeLits

infixr 6 :., :..

data RangeL :: Nat -> Nat -> * -> * where
	NilL :: RangeL 0 m a
	(:..) :: 1 <= m => a -> RangeL 0 (m - 1) a -> RangeL 0 m a
	(:.) :: a -> RangeL (n - 1) (m - 1) a -> RangeL n m a

deriving instance Show a => Show (RangeL n m a)

infixr 5 .:..

{-
class PushL m where
	(.:..) :: a -> RangeL n m a -> RangeL n (m + 1) a

-- instance {-# INCOHERENT #-} PushL 0 where
instance PushL 0 where
	x .:.. NilL = x :.. NilL
	_ .:.. _ = error "never occur"

instance {-# OVERLAPPABLE #-} (1 <= m, 1 <= m + 1, PushL (m - 1)) => PushL m where
-- instance {-# INCOHERENT #-} (1 <= m, 1 <= m + 1, PushL (m - 1)) => PushL m where
-- instance {-# OVERLAPPABLE #-} (1 <= m, PushL (m - 1)) => PushL m where
	x .:.. NilL = x :.. NilL
	x .:.. ya@(_ :.. _) = x :.. ya
	x .:.. (y :. ys) = x :. (y .:.. ys)
	-}

{-
(.:..) :: (1 <= m, 1 <= m - 1, 1 <= (m - 1) - 1) => a -> RangeL n (m - 1) a -> RangeL n m a
x .:.. NilL = x :.. NilL
x .:.. ya@(_ :.. _) = x :.. ya
x .:.. (y :. ys) = x :. (y .:.. ys)
-}

type Node = RangeL 2 3


-- type family Mm m' = r | r -> m' where
type family Mm m' where
	Mm 1 = 0
	Mm 2 = 1
	Mm m' = m' - 1

class Nodes m m' where
	nodes :: RangeL 2 m a -> RangeL 1 m' (Node a)
	(.:..) :: Node a -> RangeL 1 (m' - 1) (Node a) -> RangeL 1 m' (Node a)

instance Nodes 3 1 where
--	type Mm 1 = 0
	nodes = (:. NilL)
	x .:.. _ = x :. NilL

{-
instance Nodes 4 1 where
	nodes = (:. NilL)

instance Nodes 5 1 where
	nodes = (:. NilL)
	-}

{-
instance Nodes 6 2 where
--	type Mm 2 = 1
	nodes :: forall a . RangeL 2 6 a -> RangeL 1 2 (Node a)
	nodes (a :. b :. NilL) = (a :. b :. NilL) :. NilL
	nodes (a :. b :. c :.. NilL) = (a :. b :. c :.. NilL) :. NilL
	nodes (a :. b :. c :.. d :.. NilL) = (a :. b :. NilL) :. (c :. d :. NilL) :.. NilL
	nodes (a :. b :. c :.. d :.. e :.. xs) = (a :. b :. c :.. NilL) .:.. (nodes (d :. e :. xs :: RangeL 2 3 a) :: RangeL 1 1 (Node a))
	-}

-- instance {-# OVERLAPPABLE #-} (1 <= m', 1 <= (m' - 1), Nodes (m - 3) (m' - 1)) => Nodes m m' where
instance {-# OVERLAPPABLE #-} (1 <= (m' - 1), Nodes (m - 3) (m' - 1)) => Nodes m m' where
-- instance (1 <= (m' - 1), Nodes (m - 3) (m' - 1)) => Nodes m m' where
--	type Mm m' = m' - 1
	nodes :: forall a . RangeL 2 m a -> RangeL 1 m' (Node a)
	nodes (a :. b :. NilL) = (a :. b :. NilL) :. NilL
	nodes (a :. b :. c :.. NilL) = (a :. b :. c :.. NilL) :. NilL
	nodes (a :. b :. c :.. d :.. NilL) = (a :. b :. NilL) :. (c :. d :. NilL) :.. NilL
--	nodes (a :. b :. c :.. d :.. e :.. xs) = (a :. b :. c :.. NilL) .:.. (nodes (d :. e :. xs :: RangeL 2 (m - 3) a) :: RangeL 1 (m' - 1) (Node a))
	nodes (a :. b :. c :.. d :.. e :.. xs) = (a :. b :. c :.. NilL) ..:.. (nodes (d :. e :. xs :: RangeL 2 (m - 3) a) :: RangeL 1 (m' - 1) (Node a))
		where (..:..) = (.:..) @m
--	nodes (a :. b :. c :.. d :.. e :.. xs) = (a :. b :. c :.. NilL) .:.. nodes (d :. e :. xs)
--	x .:.. NilL = x :.. NilL
--	x .:.. ya@(_ :.. _) = x :.. ya
	x .:.. (y :. ys) = x :. (y :.. ys)
