{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.Element (
	E, toValue, fromValue, toE, fromE, IsE(..), otherE, tagName, eClass
	) where

import GHC.JS.Prim (JSVal, fromJSString)
import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.Object qualified as JS.Object
import GHC.JS.Value.EventTarget qualified as JS.EventTarget
import GHC.JS.Value.Node qualified as JS.Node
import Data.Typeable (cast)
import Data.Maybe (fromJust)

data E = forall em . JS.Value.V em => E em

instance JS.Value.IsJSVal E where toJSVal (E em) = JS.Value.toJSVal em
instance JS.Value.V E where toV = JS.Node.toValue; fromV = JS.Node.fromValue

toValue :: JS.Value.V em => em -> JS.Value.Some
toValue = JS.Value.toV . E

fromValue :: JS.Value.V em => JS.Value.Some -> Maybe em
fromValue v = JS.Value.fromV v >>= \(E em) -> cast em

instance JS.Object.IsO E
instance JS.EventTarget.IsE E

instance JS.Node.IsN E where
	downCheck nd = JS.Node.nodeType nd == JS.Node.ElementNode
	downMake = E . OtherE

toE :: IsE e => e -> E
toE = fromJust . JS.Value.cast

fromE :: forall e . IsE e => E -> Maybe e
fromE em = case (JS.Value.cast em, downCheck @e em) of
	(Just e, _) -> Just e
	(_, True) -> Just . downMake $ JS.Value.toJSVal em
	_ -> Nothing

class JS.Node.IsN e => IsE e where
	downCheck :: E -> Bool; downMake :: JSVal -> e

otherE :: JSVal -> E
otherE = toE . OtherE

newtype OtherE = OtherE JSVal

instance JS.Value.IsJSVal OtherE where toJSVal (OtherE v) = v
instance JS.Value.V OtherE where toV = toValue; fromV = fromValue

instance JS.Object.IsO OtherE
instance JS.EventTarget.IsE OtherE

instance JS.Node.IsN OtherE where
	downCheck nd = JS.Node.nodeType nd == JS.Node.ElementNode
	downMake = OtherE

instance IsE OtherE where downCheck = const True; downMake = OtherE

eClass :: JS.Object.Class
eClass = JS.Object.Class js_Element

foreign import javascript "(() => { return Element; })" js_Element :: JSVal

---------------------------------------------------------------------------
--- INSTANCE PROPERTY                                                   ---
---------------------------------------------------------------------------

-- Element.tagName

tagName :: E -> String
tagName = fromJSString . js_tagName . JS.Value.toJSVal

foreign import javascript "((e) => { return e.tagName; })"
	js_tagName :: JSVal -> JSVal
