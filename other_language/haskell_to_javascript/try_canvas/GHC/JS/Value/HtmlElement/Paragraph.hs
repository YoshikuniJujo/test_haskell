{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.HtmlElement.Paragraph where

import GHC.JS.Prim
import GHC.JS.Value qualified as JS.Value

import GHC.JS.Value.Object qualified as JS.Object
import GHC.JS.Value.EventTarget qualified as JS.EventTarget
import GHC.JS.Value.Node qualified as JS.Node
import GHC.JS.Value.Element qualified as JS.Element
import GHC.JS.Value.HtmlElement qualified as JS.HtmlElement

newtype P = P JSVal

instance JS.Value.IsJSVal P where toJSVal (P v) = v

instance JS.Value.V P where
	toV = JS.HtmlElement.toV; fromV = JS.HtmlElement.fromV

instance JS.Object.IsO P
instance JS.EventTarget.IsE P

instance JS.Node.IsN P where
	downCheck x = JS.Object.toO x `JS.Object.isInstanceOf` pClass
	downMake = P

instance JS.Element.IsE P where
	downCheck x = JS.Object.toO x `JS.Object.isInstanceOf` pClass
	downMake = P

instance JS.HtmlElement.IsH P where
	downCheck x = JS.Object.toO x `JS.Object.isInstanceOf` pClass
	downMake = P

pClass :: JS.Object.Class
pClass = JS.Object.Class js_HTMLParagraphElement

foreign import javascript "(() => { return HTMLParagraphElement; })"
	js_HTMLParagraphElement :: JSVal
