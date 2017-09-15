{-# LANGUAGE ExistentialQuantification, TypeFamilies #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

type family Foo a where
	Foo Int = Bool

data family Expr a

data instance Expr Int
	= I Int
	| Expr Int :+: Expr Int
	| Expr Int :*: Expr Int

data instance Expr Bool
	= B Bool
	| forall a . Expr a :==: Expr a

{-
data family E a where
	E Int	= I Int
		| E Int :+ E Int
		| E Int :* E Int
	E Bool	= B Bool
		| forall a . E a :=: E a
		-}
