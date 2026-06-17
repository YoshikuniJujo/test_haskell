{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.HtmlCollection where

import GHC.JS.Prim (JSVal, fromJSInt, toJSInt)
import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.Object qualified as JS.Object
import GHC.JS.Value.Element qualified as JS.Element

newtype H = H { unH :: JSVal }

instance Show H where
	show h = "(" ++ JS.Object.toString (JS.Object.toO h) ++ ")"

instance JS.Value.IsJSVal H where toJSVal (H v) = v
instance JS.Value.V H where toV = JS.Object.toValue; fromV = JS.Object.fromValue

instance JS.Object.IsO H

length :: H -> IO Int
length (H h) =  fromJSInt <$> js_length h

foreign import javascript "((h) => { return h.length; })"
	js_length :: JSVal -> IO JSVal

item :: H -> Int -> IO JS.Element.E
item (H h) (toJSInt -> i) = JS.Element.otherE <$> js_item h i

foreign import javascript "((h, i) => { return h.item(i); })"
	js_item :: JSVal -> JSVal -> IO JSVal
