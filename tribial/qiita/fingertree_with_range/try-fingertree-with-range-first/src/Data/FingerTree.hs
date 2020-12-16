{-# LANGUAGE ScopedTypeVariables, InstanceSigs #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances,
	UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fplugin=Plugin.TypeCheck.Nat.Simple #-}

module Data.FingerTree where

import GHC.TypeNats

import Data.List.Range

data FingerTree a
	= Empty
	| Single a
	| Deep (DigitL a) (FingerTree (Node a)) (DigitR a)
	deriving Show

type Node = RangeL 2 3
type DigitL = RangeL 1 4
type DigitR = RangeR 1 4

reducer :: Foldable t => (a -> b -> b) -> t a -> b -> b
reducer = flip . foldr

reducel :: Foldable t => (b -> a -> b) -> b -> t a -> b
reducel = foldl

instance Foldable FingerTree where
	foldr :: forall a b . (a -> b -> b) -> b -> FingerTree a -> b
	foldr _ z Empty = z
	foldr (-<) z (Single x) = x -< z
	foldr (-<) z (Deep pr m sf) = pr -<. (m -<.. (sf -<. z))
		where
		(-<.) :: forall t . Foldable t => t a -> b -> b
		(-<.) = reducer (-<)
		(-<..) :: forall t t' .
			(Foldable t, Foldable t') => t (t' a) -> b -> b
		(-<..) = reducer (-<.)
