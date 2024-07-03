{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.HtmlElement.Canvas where

import GHC.JS.Prim
import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.Object qualified as JS.Object
import GHC.JS.Value.EventTarget qualified as JS.EventTarget
import GHC.JS.Value.Node qualified as JS.Node
import GHC.JS.Value.Element qualified as JS.Element
import GHC.JS.Value.HtmlElement qualified as JS.HtmlElement
import GHC.JS.Value.CanvasContext qualified as JS.CanvasContext

newtype C = C JSVal

instance JS.Value.IsJSVal C where toJSVal (C v) = v

instance JS.Value.V C where
	toV = JS.HtmlElement.toV; fromV = JS.HtmlElement.fromV

instance JS.Object.IsO C
instance JS.EventTarget.IsE C

instance JS.Node.IsN C where
	downCheck x = JS.Object.toO x `JS.Object.isInstanceOf` cClass
	downMake = C

instance JS.Element.IsE C where
	downCheck x = JS.Object.toO x `JS.Object.isInstanceOf` cClass
	downMake = C

instance JS.HtmlElement.IsH C where
	downCheck x = JS.Object.toO x `JS.Object.isInstanceOf` cClass
	downMake = C

cClass :: JS.Object.Class
cClass = JS.Object.Class js_HTMLCanvasElement

foreign import javascript "(() => { return HTMLCanvasElement; })"
	js_HTMLCanvasElement :: JSVal

getHeight :: C -> IO Word
getHeight (C c) = js_getHeight c

foreign import javascript "((c) => { return c.height; })"
	js_getHeight :: JSVal -> IO Word

newtype ContextType = ContextType String

pattern ContextType2d :: ContextType
pattern ContextType2d <- ContextType "2d" where ContextType2d = ContextType "2d"

getContext :: C -> ContextType -> IO (Maybe JS.CanvasContext.C)
getContext (C cvs) (ContextType (toJSString -> ctp)) = do
	cxt <- js_getContext cvs ctp
	pure case () of
		_	| isNull cxt -> Nothing
			| isUndefined cxt -> error
				"HTMLCanvasElement.getContext return undefined"
			| otherwise -> Just . JS.CanvasContext.toC
				$ JS.CanvasContext.OtherC cxt

foreign import javascript "((c, ctp) => { return c.getContext(ctp); })"
	js_getContext :: JSVal -> JSVal -> IO JSVal
