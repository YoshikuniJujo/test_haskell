{-# LANGUAGE ExistentialQuantification, DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value where

import GHC.JS.Prim
import Data.Typeable

data Some = forall v . V v => Some v deriving Typeable

class IsJSVal v where toJSVal :: v -> JSVal

class (IsJSVal v, Typeable v) => V v where
	toV :: v -> Some
	fromV :: Some -> Maybe v

	toV = Some
	fromV (Some v) = cast v

instance IsJSVal Some where toJSVal (Some v) = toJSVal v

instance V Some where toV = id; fromV = Just
