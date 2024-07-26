{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.Event.Pointer (P) where

import GHC.JS.Prim (JSVal)
import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.Object qualified as JS.Object
import GHC.JS.Value.Event qualified as JS.Event
import GHC.JS.Value.Event.UI qualified as JS.UIEvent
import GHC.JS.Value.Event.Mouse qualified as JS.MouseEvent

newtype P = P JSVal

instance JS.Value.IsJSVal P where toJSVal (P pt) = pt

instance JS.Value.V P where
	toV = JS.MouseEvent.toValue; fromV = JS.MouseEvent.fromValue

instance JS.Object.IsO P

instance JS.Event.IsE P where
	downCheck ev = JS.Object.toO ev `JS.Object.isInstanceOf` pClass
	downMake = P

pClass :: JS.Object.Class
pClass = JS.Object.Class js_PointerEvent

foreign import javascript "(() => { return PointerEvent; })"
	js_PointerEvent :: JSVal

instance JS.UIEvent.IsU P
instance JS.MouseEvent.IsM P
