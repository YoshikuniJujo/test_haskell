{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryGenerics where

import GHC.Generics
import Data.Kind

type family TypeUncons (x :: Type -> Type) :: (Type, Type -> Type) where
	TypeUncons (M1 m i a) = TypeUncons a
	TypeUncons (M1 m i (K1 j a) :*: t2) = '(a, t2)
	TypeUncons (t1 :*: t2) = TypeUncons t1

type family GetType (x :: Type -> Type) :: Type where
	GetType (K1 i a) = a
	GetType (M1 m i a) = GetType a

type family TypeList (x :: Type -> Type) :: [Type] where
	TypeList (K1 i a) = '[a]
	TypeList (M1 m i a) = TypeList a
	TypeList (M1 m i a :*: t2) = GetType a ': TypeList t2
	TypeList ((t1 :*: t2) :*: t3) = TypeList (t1 :*: t2 :*: t3)

type family TypeUntil t (ts :: [Type]) :: Maybe [Type] where
	TypeUntil _ '[] = 'Nothing
	TypeUntil t (t : ts) = 'Just '[t]
	TypeUntil t (t' : ts) = AppMaybe ((:) t') (TypeUntil t ts)

type family TypeMaybe (n :: k) (j :: i -> k) (mx :: Maybe i) :: k where
	TypeMaybe x _ 'Nothing = x
	TypeMaybe _ f ('Just y) = f y

type family AppMaybe (f :: x -> y) (mx :: Maybe x) :: Maybe y where
	AppMaybe _ 'Nothing = 'Nothing
	AppMaybe f ('Just x) = 'Just (f x)

type family FromJust (mx :: Maybe x) :: x where
	FromJust ('Just x) = x
