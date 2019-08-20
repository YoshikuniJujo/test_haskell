{-# LANGUAGE GADTs, DataKinds, TypeOperators, FlexibleInstances, FlexibleContexts, UndecidableInstances, TypeFamilies, StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fplugin=GHC.TypeLits.Normalise #-}

module Lib where

import GHC.TypeNats

list3 :: a -> a -> a -> [a]
list3 x y z = [x, y, z]

index0of3, index1of3, index2of3 :: [a] -> a
index0of3 [x, _, _] = x
index0of3 _ = error "index2of3 xs: xs should be [_, _, _]"

index1of3 [_, y, _] = y
index1of3 _ = error "index2of3 xs: xs should be [_, _, _]"

index2of3 [_, _, z] = z
index2of3 _ = error "index2of3 xs: xs should be [_, _, _]"

data Tuple n a where
	Nil :: Tuple 0 a
	(:-) :: a -> Tuple n a -> Tuple (n + 1) a

deriving instance Show a => Show (Tuple n a)

headT :: Tuple (n + 1) a -> a
headT (x :- _) = x

tailT :: Tuple (n + 1) a -> Tuple n a
tailT (_ :- xs) = xs

{-
data family Tuple :: Nat -> * -> *

data instance Tuple 0 a = Nil

data instance Tuple n a = a :- Tuple (n - 1) a
-}

infixr 5 :-

{-
instance {-# OVERLAPPING #-} Show a => Show (Tuple 0 a) where
	show Nil = "Nil"

-- instance {-# OVERLAPPABLE #-} (Show a, Show (Tuple (n - 1) a)) => Show (Tuple n a) where
--	show (x :- tpl) = show x ++ " :- " ++ show tpl

instance {-# OVERLAPPABLE #-} Show a => Show (Tuple 1 a) where
	show (x :- tpl) = show x ++ show tpl
	-}
