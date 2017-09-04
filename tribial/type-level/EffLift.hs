{-# LANGUAGE ExistentialQuantification, TypeFamilies #-}
{-# LANGUAGE KindSignatures, DataKinds, TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

{-# OPTIONS_GHC -Wall
	-fno-warn-tabs
	-fno-warn-simplifiable-class-constraints #-}

module EffLift (Lift(..), OnlyLift) where

import EffList

class (OneLift m ms ~ 'True) => OnlyLift m ms
instance (OneLift m ms ~ 'True) => OnlyLift m ms

data Lift m v = forall a . Lift (m a) (a -> v)

instance Functor (Lift m) where
	fmap f (Lift m k) = Lift m (f . k)

type family IsLifted m where
	IsLifted (Lift m) = 'True
	IsLifted m = 'False

type family ElemLiftGen (b :: Bool) (ms :: Effs) where
	ElemLiftGen 'True ms = 'True
	ElemLiftGen 'False (m :> ms) = ElemLiftGen (IsLifted m) ms
	ElemLiftGen b Emp = b

type ElemLift ms = ElemLiftGen 'False ms

type family OneLiftGen (b :: Bool) m ms where
	OneLiftGen 'True m ms = 'False
	OneLiftGen 'False t (t :> ms) = Not (ElemLift ms)
	OneLiftGen 'False t (t' :> ms) = OneLiftGen (IsLifted t) t ms
	OneLiftGen 'False t Emp = 'False

type family Not t where
	Not 'False = 'True
	Not 'True = 'False

type OneLift m ms = OneLiftGen 'False m ms
