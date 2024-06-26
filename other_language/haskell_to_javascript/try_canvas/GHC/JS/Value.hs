{-# LANGUAGE ExistentialQuantification, DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value where

import GHC.JS.Prim
import Data.Typeable qualified as Tp

data Some = forall v . V v => Some v

class IsJSVal v where toJSVal :: v -> JSVal

class (IsJSVal v, Tp.Typeable v) => V v where
	toV :: v -> Some
	fromV :: Some -> Maybe v

	toV = Some
	fromV (Some v) = Tp.cast v

instance IsJSVal Some where toJSVal (Some v) = toJSVal v

instance V Some where toV = id; fromV = Just

cast :: (V v1, V v2) => v1 -> Maybe v2
cast = fromV . toV
