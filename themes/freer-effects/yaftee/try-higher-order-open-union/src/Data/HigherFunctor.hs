{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.HigherFunctor (Loose(..), TightI(..), TightO(..), Tight(..)) where

import Prelude hiding (map)
import Control.HigherOpenUnion

class Loose h where
	map :: (forall i o x . f i o x -> g i o (t x)) ->
		(forall x . x -> t x) -> h f i' o' x' -> h g i' o' (t x')
	default map :: Tight h => (forall i o x . f i o x -> g i o (t x)) ->
		(forall x . x -> t x) -> h f i' o' x' -> h g i' o' (t x')
	map = mapT

instance Loose (FromFirst t)
instance Loose (U '[])

instance (Loose h, Loose (U hs)) => Loose (U (h ': hs)) where
	map f g u = case decomp u of
		Left u' -> weaken $ map f g u'
		Right h -> injh $ map f g h

class TightI h where
	mapI :: (forall o x . f i o x -> g i' o (t x)) ->
		(forall x . x -> t x) -> h f i o' x' -> h g i' o' (t x')
	default mapI :: Tight h => (forall o x . f i o x -> g i' o (t x)) ->
		(forall x . x -> t x) -> h f i o' x' -> h g i' o' (t x')
	mapI = mapT

instance TightI (FromFirst t)
instance TightI (U '[])

instance (TightI h, TightI (U hs)) => TightI (U (h ': hs)) where
	mapI f g u = case decomp u of
		Left u' -> weaken $ mapI f g u'
		Right h -> injh $ mapI f g h

class TightO h where
	mapO :: (forall i x . f i o x -> g i o' (t x)) ->
		(forall x . x -> t x) -> h f i' o x' -> h g i' o' (t x')
	default mapO :: Tight h => (forall i x . f i o x -> g i o' (t x)) ->
		(forall x . x -> t x) -> h f i' o x' -> h g i' o' (t x')
	mapO = mapT

instance TightO (FromFirst t)
instance TightO (U '[])

instance (TightO h, TightO (U hs)) => TightO (U (h ': hs)) where
	mapO f g u = case decomp u of
		Left u' -> weaken $ mapO f g u'
		Right h -> injh $ mapO f g h

class Tight h where
	mapT :: (f i o x -> g i' o' y) -> (x -> y) -> h f i o x -> h g i' o' y

instance Tight (FromFirst t) where
	mapT _ g (FromFirst x k) = FromFirst x (g . k)

instance Tight (U '[]) where mapT _ _ _ = error "bad"

instance (Tight h, Tight (U hs)) => Tight (U (h ': hs)) where
	mapT f g u = case decomp u of
		Left u' -> weaken $ mapT f g u'
		Right h -> injh $ mapT f g h
