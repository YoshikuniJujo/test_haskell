{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Yaftee.HFunctorNew where

import Yaftee.OpenUnion

class HFunctor h where
	hmap :: (f i o x -> g i o x) -> h f i o x -> h g i o x

instance HFunctor (FromFirst t) where hmap _ (FromFirst x k) = FromFirst x k

instance HFunctor (U '[]) where hmap _ _ = error "bad"

instance (HFunctor h, HFunctor (U hs)) => HFunctor (U (h ': hs)) where
	hmap f u = case decomp u of
		Left u' -> weaken $ hmap f u'
		Right h -> injh $ hmap f h
