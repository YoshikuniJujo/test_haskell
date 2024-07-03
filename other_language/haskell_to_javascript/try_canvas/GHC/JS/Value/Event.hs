{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.Event where

import GHC.JS.Prim
import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.Object qualified as JS.Object
import Data.Typeable
import Data.Maybe

data E = forall ev . JS.Value.V ev => E ev

instance JS.Value.IsJSVal E where toJSVal (E ev) = JS.Value.toJSVal ev
instance JS.Value.V E where toV = JS.Object.toV; fromV = JS.Object.fromV

toV :: JS.Value.V ev => ev -> JS.Value.Some
toV = JS.Value.toV . E

fromV :: JS.Value.V ev => JS.Value.Some -> Maybe ev
fromV v = JS.Value.fromV v >>= \(E ev) -> cast ev

class JS.Object.IsO ev => IsE ev where
	downCheck :: E -> Bool; downMake :: JSVal -> ev

toE :: IsE ev => ev -> E
toE = fromJust . JS.Value.cast

fromE :: forall ev . IsE ev => E -> Maybe ev
fromE ev = case (JS.Value.cast ev, downCheck @ev ev) of
	(Just d, _) -> Just d
	(Nothing, True) -> Just . downMake $ JS.Value.toJSVal ev
	_ -> Nothing

newtype OtherE = OtherE JSVal

instance JS.Value.IsJSVal OtherE where toJSVal (OtherE v) = v
instance JS.Value.V OtherE where toV = toV; fromV = fromV

instance JS.Object.IsO OtherE

instance IsE OtherE where downCheck = const True; downMake = OtherE
