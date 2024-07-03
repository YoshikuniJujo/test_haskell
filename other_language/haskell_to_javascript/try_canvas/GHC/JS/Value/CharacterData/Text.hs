{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.CharacterData.Text where

import GHC.JS.Prim
import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.Object qualified as JS.Object
import GHC.JS.Value.EventTarget qualified as JS.EventTarget
import GHC.JS.Value.Node qualified as JS.Node
import GHC.JS.Value.CharacterData qualified as JS.CharacterData

newtype T = T JSVal

instance JS.Value.IsJSVal T where toJSVal (T v) = v
instance JS.Value.V T where
	toV = JS.CharacterData.toV; fromV = JS.CharacterData.fromV

instance JS.Object.IsO T
instance JS.EventTarget.IsE T

instance JS.Node.IsN T where
	downCheck x = JS.Object.toO x `JS.Object.isInstanceOf` tClass
	downMake = T

instance JS.CharacterData.IsC T where
	downCheck x = JS.Object.toO x `JS.Object.isInstanceOf` tClass
	downMake = T

tClass :: JS.Object.Class
tClass = JS.Object.Class js_Text

foreign import javascript "(() => { return Text; })" js_Text :: JSVal

new :: String -> IO T
new t = T <$> js_newText (toJSString t)

foreign import javascript "((t) => { return new Text(t); })"
	js_newText :: JSVal -> IO JSVal
