{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.CanvasContext (
	C, toValue, fromValue, toC, fromC, IsC(..), otherC ) where

import GHC.JS.Prim (JSVal)
import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.Object qualified as JS.Object
import Data.Typeable (cast)
import Data.Maybe (fromJust)

data C = forall cc . JS.Value.V cc => C cc

instance JS.Value.IsJSVal C where toJSVal (C cc) = JS.Value.toJSVal cc
instance JS.Value.V C where toV = JS.Object.toValue; fromV = JS.Object.fromValue

toValue :: JS.Value.V cc => cc -> JS.Value.Some
toValue = JS.Value.toV . C

fromValue :: JS.Value.V cc => JS.Value.Some -> Maybe cc
fromValue v = JS.Value.fromV v >>= \(C cc) -> cast cc

instance JS.Object.IsO C

toC :: IsC cc => cc -> C
toC = fromJust . JS.Value.cast

fromC :: forall cc . IsC cc => C -> Maybe cc
fromC cc = case (JS.Value.cast cc, downCheck @cc cc) of
	(Just cc', _) -> Just cc'
	(_, True) -> Just . downMake $ JS.Value.toJSVal cc
	_ -> Nothing

class JS.Object.IsO cc => IsC cc where
	downCheck :: C -> Bool; downMake :: JSVal -> cc

otherC :: JSVal -> C
otherC = toC . OtherC

newtype OtherC = OtherC JSVal

instance JS.Value.IsJSVal OtherC where toJSVal (OtherC v) = v
instance JS.Value.V OtherC where toV = toValue; fromV = fromValue

instance JS.Object.IsO OtherC
instance IsC OtherC where downCheck = const True; downMake = OtherC
