{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Yaftee.HFunctor where

import Yaftee.OpenUnion

class HFunctor h where
	hmap :: (f i o x -> g i' o' y) -> (x -> y) -> h f i o x -> h g i' o' y

instance HFunctor (FromFirst t) where
	hmap _ g (FromFirst x h) = FromFirst x (g . h)

instance HFunctor (U '[]) where hmap _ _ _ = error "bad"

instance (HFunctor h, HFunctor (U hs)) => HFunctor (U (h ': hs)) where
--	hmap :: forall f g i o x y .
--		(f i o x -> g i o y) -> (x -> y) -> h f i o x -> h g i o y
	hmap f g u = case decomp u of
		Left u' -> weaken $ hmap f g u'
		Right h -> injh $ hmap f g h
