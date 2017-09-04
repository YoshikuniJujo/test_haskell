{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lift where

data Lift :: (* -> *) -> (* -> *)

type family Unlift m where
	Unlift (Lift m) = m

type family IsLifted m where
	IsLifted (Lift m) = 'True
	IsLifted m = 'False

type family ElemLiftGen (b :: Bool) (ms :: [* -> *]) where
	ElemLiftGen 'True ms = 'True
	ElemLiftGen 'False (m : ms) = ElemLiftGen (IsLifted m) ms
	ElemLiftGen b '[] = b

type ElemLift ms = ElemLiftGen 'False ms

type family OnlyOneLiftGen (b :: Bool) (ms :: [* -> *]) where
	OnlyOneLiftGen 'True ms = Not (ElemLift ms)
	OnlyOneLiftGen 'False (m : ms) = OnlyOneLiftGen (IsLifted m) ms
	OnlyOneLiftGen b '[] = b

type family Not (b :: Bool) where
	Not 'False = 'True
	Not 'True = 'False

type OnlyOneLift ms = OnlyOneLiftGen 'False ms

testUnlift :: Unlift (Lift IO) Integer
testUnlift = return 8

data Id (r :: [* -> *]) a = Id a deriving Show

testElemLift :: ElemLift ms ~ 'True => Id ms Int -> Id ms Int
testElemLift = id

testOnlyOneLift :: OnlyOneLift ms ~ 'True => Id ms Int -> Id ms Int
testOnlyOneLift = id
