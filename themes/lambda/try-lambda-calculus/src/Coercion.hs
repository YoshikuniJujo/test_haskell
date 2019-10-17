{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Coercion where

data Type
	= Type String
	| TApp Type Type
	deriving (Show, Eq)

data Coercion = Coercion Type Type deriving Show

reflection :: Type -> Coercion
reflection t = Coercion t t

symmetry :: Coercion -> Coercion
symmetry (Coercion t1 t2) = Coercion t2 t1

transitivity :: Coercion -> Coercion -> Maybe Coercion
transitivity (Coercion t1 t2) (Coercion t2' t3)
	| t2 == t2' = Just $ Coercion t1 t3
	| otherwise = Nothing

composition :: Coercion -> Coercion -> Coercion
composition (Coercion f1 f2) (Coercion t1 t2) =
	Coercion (f1 `TApp` t1) (f2 `TApp` t2)

left, right :: Coercion -> Maybe Coercion
left (Coercion (f1 `TApp` _t1) (f2 `TApp` _t2)) = Just $ Coercion f1 f2
left _ = Nothing

right (Coercion (_f1 `TApp` t1) (_f2 `TApp` t2)) = Just $ Coercion t1 t2
right _ = Nothing
