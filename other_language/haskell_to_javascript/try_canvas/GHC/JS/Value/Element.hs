{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.Element where

import GHC.JS.Prim
import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.Object qualified as JS.Object
import GHC.JS.Value.EventTarget qualified as JS.EventTarget
import GHC.JS.Value.Node qualified as JS.Node
import Data.Typeable
import Data.Maybe

data E = forall em . JS.Value.V em => E em

instance JS.Value.IsJSVal E where toJSVal (E em) = JS.Value.toJSVal em
instance JS.Value.V E where toV = JS.Node.toV; fromV = JS.Node.fromV

toV :: JS.Value.V em => em -> JS.Value.Some
toV = JS.Value.toV . E

fromV :: JS.Value.V em => JS.Value.Some -> Maybe em
fromV v = JS.Value.fromV v >>= \(E em) -> cast em

instance JS.Object.IsO E
instance JS.EventTarget.IsE E

instance JS.Node.IsN E where
	downCheck nd = JS.Node.getNodeType nd == JS.Node.ElementNode
	downMake = E . OtherE

toE :: IsE e => e -> E
toE = fromJust . JS.Value.cast

fromE :: forall e . IsE e => E -> Maybe e
fromE em = case (JS.Value.cast em, downCheck @e em) of
	(Just d, _) -> Just d
	(_, True) -> Just . downMake $ JS.Value.toJSVal em
	_ -> Nothing

class JS.Node.IsN e => IsE e where
	downCheck :: E -> Bool; downMake :: JSVal -> e

newtype OtherE = OtherE JSVal

instance JS.Value.IsJSVal OtherE where toJSVal (OtherE v) = v
instance JS.Value.V OtherE where toV = toV; fromV = fromV

instance JS.Object.IsO OtherE
instance JS.EventTarget.IsE OtherE

instance JS.Node.IsN OtherE where
	downCheck nd = JS.Node.getNodeType nd == JS.Node.ElementNode
	downMake = OtherE

instance IsE OtherE where downCheck = const True; downMake = OtherE

getTagName :: E -> String
getTagName = fromJSString . js_getTagName . JS.Value.toJSVal

foreign import javascript "((e) => { return e.tagName; })"
	js_getTagName :: JSVal -> JSVal

eClass :: JS.Object.Class
eClass = JS.Object.Class js_Element

foreign import javascript "(() => { return Element; })" js_Element :: JSVal
