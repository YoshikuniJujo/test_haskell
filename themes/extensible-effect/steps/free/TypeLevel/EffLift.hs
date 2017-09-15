{-# LANGUAGE ExistentialQuantification, TypeFamilies #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TypeLevel.EffLift (Lift(..), BaseLift) where

import TypeLevel.EffList

data Lift m v = forall a . Lift (m a) (a -> v)

instance Functor (Lift m) where
	fmap f (Lift m k) = Lift m (f . k)

type family IsLifted m where
	IsLifted (Lift m) = 'True
	IsLifted m = 'False

class (BaseLiftGen 'False m ms ~ 'True) => BaseLift m ms
instance (BaseLiftGen 'False m ms ~ 'True) => BaseLift m ms

type family BaseLiftGen (b :: Bool) m ms where
	BaseLiftGen 'True m ms = 'False
	BaseLiftGen 'False m (m :> Emp) = 'True
	BaseLiftGen 'False m (m' :> ms) = BaseLiftGen (IsLifted m') m ms
	BaseLiftGen 'False m Emp = 'False
