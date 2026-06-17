{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.Object (
	O, new, toValue, fromValue, IsO, toO,
	isInstanceOf, Class(..), toString, consoleLog, set ) where

import GHC.JS.Prim (JSVal, fromJSString, toJSString, toJSInt)
import GHC.JS.Value qualified as JS.Value
import Data.Typeable (cast)
import Data.Maybe (fromJust)

data O = forall o . JS.Value.V o => O o

instance Show O where show = toString

instance JS.Value.IsJSVal O where toJSVal (O o) = JS.Value.toJSVal o
instance JS.Value.V O

toValue :: JS.Value.V o => o -> JS.Value.Some
toValue = JS.Value.toV . O

fromValue :: JS.Value.V o => JS.Value.Some -> Maybe o
fromValue v = JS.Value.fromV v >>= \(O o) -> cast o

toO :: IsO o => o -> O
toO = fromJust . JS.Value.cast

class JS.Value.V o => IsO o

isInstanceOf :: O -> Class -> Bool
o `isInstanceOf` Class c = JS.Value.toJSVal o `js_instanceof` c

foreign import javascript "((o, c) => { return (o instanceof c); })"
	js_instanceof :: JSVal -> JSVal -> Bool

newtype Class = Class JSVal

toString :: O -> String
toString obj = fromJSString . js_toString $ JS.Value.toJSVal obj

foreign import javascript "((o) => { return o.toString(); })"
	js_toString :: JSVal -> JSVal

consoleLog :: O -> IO ()
consoleLog o = js_consoleLog $ JS.Value.toJSVal o

foreign import javascript "((o) => { console.log(o); })"
	js_consoleLog :: JSVal -> IO ()

new :: IO O
new = toO . OtherO <$> js_new

foreign import javascript "(() => { return {} })" js_new :: IO JSVal

set :: O -> String -> O -> IO ()
set (JS.Value.toJSVal -> o) (toJSString -> k) (JS.Value.toJSVal -> v) =
	js_set o k v

foreign import javascript "((o, k, v) => { o[k] = v; })"
	js_set :: JSVal -> JSVal -> JSVal -> IO ()

newtype OtherO = OtherO JSVal

instance JS.Value.IsJSVal OtherO where toJSVal (OtherO v) = v
instance JS.Value.V OtherO where toV = toValue; fromV = fromValue

instance IsO OtherO

instance JS.Value.IsJSVal String where toJSVal = toJSString
instance JS.Value.V String where toV = toValue; fromV = fromValue

instance IsO String

instance JS.Value.IsJSVal Int where toJSVal = toJSInt
instance JS.Value.V Int where toV = toValue; fromV = fromValue

instance IsO Int
