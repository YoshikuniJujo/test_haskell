{-# LANGUAGE FlexibleInstances, GADTs, InstanceSigs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-orphans #-}

module Lib where

import Prelude hiding ((.), id)

import Control.Category

instance Category c => Semigroup (c () ()) where
	(<>) = (.)

instance Category c => Monoid (c () ()) where
	mempty = id

data MonoidCat m a b where
	MonoidCat :: m -> MonoidCat m () ()
	MonoidCatId :: MonoidCat m a a

instance Monoid m => Category (MonoidCat m) where
	MonoidCat a . MonoidCat b = MonoidCat $ a <> b
	MonoidCat a . MonoidCatId = MonoidCat $ a <> mempty
	MonoidCatId . MonoidCat b = MonoidCat $ mempty <> b
	MonoidCatId . MonoidCatId = MonoidCat $ mempty <> mempty
	id = MonoidCatId
