{-# LANGUAGE GADTs, TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TList where

import Prelude hiding ((.))

import Control.Category
import Data.Char

infixr 5 :^

data TList c x y where
	Nil :: TList c x x
	(:^) :: c x y -> TList c y z -> TList c x z

data Fun a b = Fun Integer (a -> b)

sampleFunList :: TList Fun Char Int
sampleFunList = Fun 0 ord :^ Fun 1 (+ 15) :^ Nil

sample :: TList (->) Char Int
sample = ord :^ (+ 15) :^ Nil

apply :: TList (->) a b -> a -> b
apply Nil x = x
apply (f :^ fs) x = apply fs $ f x

data AsUnitLoop a b c where UL :: a -> AsUnitLoop a () ()

newtype AsSequence s a = AS (s (AsUnitLoop a) () ())

class Sequence s where
	empty :: s a
	singleton :: a -> s a
	(><^) :: s a -> s a -> s a
	viewl :: s a -> ViewL s a

data ViewL s a where
	EmptyL :: ViewL s a
	(:<|^) :: a -> s a -> ViewL s a

class TSequence s where
	tempty :: s c x x
	tsingleton :: c x y -> s c x y
	(><) :: s c x y -> s c y z -> s c x z
	tviewl :: s c x y -> TViewl s c x y

data TViewl s c x y where
	TEmptyL :: TViewl s c x x
	(:<|) :: c x y -> s c y z -> TViewl s c x z
	
instance TSequence s => Sequence (AsSequence s) where
	empty = AS tempty
	singleton = AS . tsingleton . UL
	AS x ><^ AS y = AS $ x >< y
	viewl (AS s) = case tviewl s of
		TEmptyL -> EmptyL
		UL h :<| t -> h :<|^ AS t

fromList :: [a] -> AsSequence TList a
fromList [] = AS Nil
fromList (x : xs) = let AS xs' = fromList xs in AS $ UL x :^ xs'

instance Sequence s => Semigroup (s a) where (<>) = (><^)
instance Sequence s => Monoid (s a) where mempty = empty
instance TSequence s => Category (s c) where id = tempty; (.) = flip (><)

{-
data AsUnit a b c where U :: a -> AsUnit a () ()

instance Monoid a => Category (AsUnit a) where
	id = U mempty
	U x . U y = U $ x <> y
	-}
