{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryGenerics where

import GHC.Generics

type family TypeUncons (x :: * -> *) :: (*, * -> *) where
	TypeUncons (M1 m i a) = TypeUncons a
	TypeUncons (M1 m i (K1 j a) :*: t2) = '(a, t2)
	TypeUncons (t1 :*: t2) = TypeUncons t1

type family TypeList (x :: * -> *) :: [*] where
	TypeList (K1 i a) = '[a]
	TypeList (M1 m i a) = TypeList a
	TypeList (t1 :*: t2) = TypeList t2
